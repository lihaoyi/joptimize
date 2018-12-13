package joptimize

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes, Type}
import org.objectweb.asm.tree.{AbstractInsnNode, ClassNode, MethodInsnNode, MethodNode}
import org.objectweb.asm.tree.analysis.{Analyzer, Frame}

import collection.JavaConverters._
import scala.collection.mutable
object JOptimize{
  def run(classFiles: Map[String, Array[Byte]],
          entrypoints: Seq[MethodSig],
          eliminateOldMethods: Boolean) = {

    val classFileMap = for((k, v) <- classFiles) yield {
      val cr = new ClassReader(v)
      val cn = new ClassNode()
      cr.accept(cn, ClassReader.SKIP_FRAMES)
      (k, cn)
    }

    val classNodeMap = classFileMap.map{case (k, v) => (v.name, v)}

    val originalMethods = for{
      (k, cls) <- classNodeMap
      m <- cls.methods.iterator().asScala
    } yield (MethodSig(cls.name, m.name, m.desc, (m.access & Opcodes.ACC_STATIC) != 0), m)

    val newMethods = mutable.Buffer.empty[(ClassNode, MethodNode)]
    val visited = collection.mutable.Map.empty[(MethodSig, Option[Seq[Type]]), Type]
    for(ep <- entrypoints){
      visited(ep -> None) = Type.getType(ep.desc).getReturnType
    }
    def recurse(sig: MethodSig, mangledTypes: Option[Seq[Type]]): Type = {
      visited.getOrElseUpdate((sig, mangledTypes), {
        val cn = classNodeMap(sig.clsName)
        mangledTypes match{
          case None =>
            process(cn, originalMethods(sig), classNodeMap, recurse)
          case Some(mangled) =>
            val originalMethod = originalMethods(sig)
            val (mangledName, mangledDesc) = mangle(
              originalMethod.name,
              mangled,
              Type.getType(originalMethod.desc).getReturnType
            )

            val mangledMethodNode = new MethodNode(
              Opcodes.ASM6,
              originalMethod.access,
              mangledName,
              mangledDesc,
              originalMethod.signature,
              originalMethod.exceptions.asScala.toArray
            )

            originalMethod.accept(mangledMethodNode)

            val returnType = process(cn, mangledMethodNode, classNodeMap, recurse)

            mangledMethodNode.desc = Type.getMethodDescriptor(
              returnType,
              Type.getType(mangledMethodNode.desc).getArgumentTypes:_*
            )

            newMethods.append((cn, mangledMethodNode))
            returnType
        }
      })
    }

    for(entrypoint <- entrypoints){
      process(
        classNodeMap(entrypoint.clsName),
        originalMethods(entrypoint),
        classNodeMap,
        recurse
      )
    }

    if (eliminateOldMethods) {
      for ((k, cn) <- classFileMap) {
        cn.methods.removeIf { mn =>
          val sig = MethodSig(cn.name, mn.name, mn.desc, (mn.access & Opcodes.ACC_STATIC) != 0)
          !visited.keys.exists(x => x._1 == sig && x._2.isEmpty) && mn.name != "<init>" && mn.name != "<clinit>"
        }
      }
    }

    for((cn, mn) <- newMethods) cn.methods.add(mn)

    for((k, cn) <- classFileMap) yield {
      val cw = new ClassWriter(Opcodes.ASM6)
      cn.accept(cw)
      (k, cw.toByteArray)
    }
  }

  /**
    * Transforms a single method, narrowing any method calls within that method
    * and returning a list of called methods that would need to be processed next
    */
  def process(cn: ClassNode,
              mn: MethodNode,
              classNodeMap: Map[String, ClassNode],
              recurse: (MethodSig, Option[Seq[Type]]) => Type): Type = {
    val df = new Dataflow()
    val analyzer = new Analyzer(df)
    analyzer.analyze(cn.name, mn)
    val frames = analyzer.getFrames
    val narrowReturnTypes = mutable.Buffer.empty[Inferred]
    val originalMethodType = Type.getType(mn.desc)
    var narrowReturn = false
    for((insn, idx) <- mn.instructions.iterator().asScala.zipWithIndex) {
      insn.getOpcode match{
        case Opcodes.INVOKEVIRTUAL | Opcodes.INVOKEINTERFACE | Opcodes.INVOKESPECIAL =>
          mangleInstruction(
            frames, insn, idx,
            static = false,
            isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
            replace = mn.instructions.set,
            recurse = recurse
          )
        case Opcodes.INVOKESTATIC =>
          mangleInstruction(
            frames, insn, idx,
            static = true,
            isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
            replace = mn.instructions.set,
            recurse = recurse
          )
        case Opcodes.ARETURN =>
          val inferred = frames(idx).getStack(frames(idx).getStackSize - 1)
          if (inferred.value != originalMethodType.getReturnType) {
            narrowReturn = true
            narrowReturnTypes.append(inferred)
          }
        case _ => // do nothing
      }
    }
    if (narrowReturn){
      val narrowReturnType = narrowReturnTypes.reduce(df.merge).value
      mn.desc = Type.getMethodDescriptor(narrowReturnType, originalMethodType.getArgumentTypes:_*)
      narrowReturnType
    }else {
      originalMethodType.getReturnType
    }
  }


  def mangleInstruction(frames: Array[Frame[Inferred]],
                        insn: AbstractInsnNode,
                        idx: Int,
                        static: Boolean,
                        isInterface: String => Boolean,
                        replace: (AbstractInsnNode, AbstractInsnNode) => Unit,
                        recurse: (MethodSig, Option[Seq[Type]]) => Type) = {
    val called = insn.asInstanceOf[MethodInsnNode]

    val calledDesc = Type.getType(called.desc)
    val calledTypes = calledDesc.getArgumentTypes.toSeq

    val frame = frames(idx)
    val stackTypes =
      (frame.getStackSize - calledTypes.length)
        .until(frame.getStackSize)
        .map(frame.getStack(_).value)

    val sig = MethodSig(called.owner, called.name, called.desc, static)
    if (stackTypes == calledTypes) {
      if (static) recurse(sig, None)
      else if (Type.getObjectType(called.owner) != frame.getStack(frame.getStackSize - calledTypes.length).value){
        val newOwner = frame.getStack(frame.getStackSize - calledTypes.length - 1).value.getInternalName
        (isInterface(called.owner), isInterface(newOwner)) match{
          case (true, false) =>
            val newNode = new MethodInsnNode(Opcodes.INVOKEVIRTUAL, newOwner, called.name, called.desc)
            replace(insn, newNode)
            recurse(sig.copy(clsName = newOwner), None)
          case (false, true) => ???
          case _ =>
            called.owner = newOwner
            recurse(sig, None)
        }
      }else{
        recurse(sig, None)
      }
    } else {
      val narrowReturnType = recurse(sig, Some(stackTypes))
      val (mangledName, mangledDesc) = mangle(called.name, stackTypes, narrowReturnType)
      called.name = mangledName
      called.desc = mangledDesc
    }
  }

  def mangle(name: String, stackTypes: Seq[Type], narrowReturnType: Type) = {
    val mangledName = name + "__" + stackTypes.mkString("__").replace('/', '_').replace(';', '_')
    val mangledDesc = Type.getMethodDescriptor(narrowReturnType, stackTypes:_*)
    (mangledName, mangledDesc)
  }
}
