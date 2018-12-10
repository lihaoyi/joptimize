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
    val remaining = mutable.Queue.empty[(MethodSig, Option[Seq[Type]])]
    remaining.enqueue(entrypoints.map((_, None)):_*)
    val visited = collection.mutable.Set.empty[(MethodSig, Option[Seq[Type]])]
    while(remaining.nonEmpty){
      val current = remaining.dequeue()
      if (!visited.contains(current)){
        visited.add(current)
        val (sig, mangledTypes) = current
        val cn = classNodeMap(sig.clsName)
        val mn = mangledTypes match{
          case None => originalMethods(sig)
          case Some(mangled) =>
            val originalMethod = originalMethods(sig)
            val (mangledName, mangledDesc) = mangle(
              originalMethod.name,
              Type.getType(originalMethod.desc),
              mangled
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

            newMethods.append((cn, mangledMethodNode))
            mangledMethodNode
        }
        remaining.enqueue(process(cn, mn, classNodeMap):_*)
      }
    }

    for((k, cn) <- classFileMap) {
      cn.methods.removeIf { mn =>
        val sig = MethodSig(cn.name, mn.name, mn.desc, (mn.access & Opcodes.ACC_STATIC) != 0)
        val res = !visited.exists(x => x._1 == sig && x._2.isEmpty) && mn.name != "<init>" && mn.name != "<clinit>"
        res
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
              classNodeMap: Map[String, ClassNode]): Seq[(MethodSig, Option[Seq[Type]])] = {
    val df = new Dataflow()
    val analyzer = new Analyzer(df)
    analyzer.analyze(cn.name, mn)
    val frames = analyzer.getFrames
    val output = mutable.Buffer.empty[(MethodSig, Option[Seq[Type]])]
    for((insn, idx) <- mn.instructions.iterator().asScala.zipWithIndex) {
      insn.getOpcode match{
        case Opcodes.INVOKEVIRTUAL | Opcodes.INVOKEINTERFACE | Opcodes.INVOKESPECIAL =>
          mangleInstruction(
            frames, output, insn, idx,
            static = false,
            isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
            replace = mn.instructions.set
          )
        case Opcodes.INVOKESTATIC =>
          mangleInstruction(
            frames, output, insn, idx,
            static = true,
            isInterface = s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
            replace = mn.instructions.set
          )
        case _ => // do nothing
      }
    }
    output
  }


  def mangleInstruction(frames: Array[Frame[Inferred]],
                        output: mutable.Buffer[(MethodSig, Option[Seq[Type]])],
                        insn: AbstractInsnNode,
                        idx: Int,
                        static: Boolean,
                        isInterface: String => Boolean,
                        replace: (AbstractInsnNode, AbstractInsnNode) => Unit) = {
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
      if (static) output.append((sig, None))
      else if (Type.getObjectType(called.owner) != frame.getStack(frame.getStackSize - calledTypes.length).value){
        val newOwner = frame.getStack(frame.getStackSize - calledTypes.length - 1).value.getInternalName
        (isInterface(called.owner), isInterface(newOwner)) match{
          case (true, false) =>
            val newNode = new MethodInsnNode(Opcodes.INVOKEVIRTUAL, newOwner, called.name, called.desc)
            replace(insn, newNode)
            output.append((sig.copy(clsName = newOwner), None))
          case (false, true) => ???
          case _ =>
            called.owner = newOwner
            output.append((sig, None))
        }



      }else{
        output.append((sig, None))
      }
    } else {
      val (mangledName, mangledDesc) = mangle(called.name, calledDesc, stackTypes)
      output.append((sig, Some(stackTypes)))
      called.name = mangledName
      called.desc = mangledDesc
    }
  }

  def mangle(name: String, desc: Type, stackTypes: Seq[Type]) = {
    val mangledName = name + "__" + stackTypes.mkString("__").replace('/', '_').replace(';', '_')
    val mangledDesc = Type.getMethodDescriptor(desc.getReturnType, stackTypes:_*)
    (mangledName, mangledDesc)
  }
}
