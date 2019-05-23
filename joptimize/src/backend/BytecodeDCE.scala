package joptimize.backend

import joptimize.frontend.ClassManager
import joptimize.{Logger, Util, frontend}
import joptimize.model.{Desc, JType, MethodSig, SSA}
import org.objectweb.asm.tree._
import org.objectweb.asm.{Handle, Opcodes}

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Post-liveness cleanup phase to remove any classes which have been rendered
  * unreachable by the liveness analysis. We cannot do this during the initial
  * walk because we need all liveness analysis to be complete before we can
  * scan for new unreachability.
  */
object BytecodeDCE {
  def apply(entrypoints: scala.Seq[MethodSig],
            classNodes: Seq[ClassNode],
            ignore: String => Boolean,
            log: Logger.Global): Seq[ClassNode] = log.block{

    val classNodeMap = classNodes.map(c => (c.name, c)).toMap
    val classManager = new frontend.ClassManager.Dynamic(classNodeMap.get(_))
    val queue = entrypoints.to[mutable.Queue]
    val seenMethods = mutable.LinkedHashSet.empty[MethodSig]
    val seenClasses = mutable.LinkedHashSet.empty[JType.Cls]
    val virtualCallSignatures = mutable.LinkedHashMap.empty[JType.Cls, List[MethodSig]]
    def addSeenClassAndClInit(cls: JType.Cls) = {
      seenClasses.add(cls)
      val clinitMethod = MethodSig(cls, "<clinit>", Desc.read("()V"), true)
      if (classManager.loadMethod(clinitMethod).nonEmpty) queue.enqueue(clinitMethod)
    }
    seenClasses.add("scala/runtime/Nothing$")
    seenClasses.add("scala/runtime/Null$")

    while(queue.nonEmpty){
      val currentSig = queue.dequeue()
      if (!ignore(currentSig.cls.name) && !seenMethods(currentSig)){
        seenMethods.add(currentSig)
        seenClasses.add(currentSig.cls)

        val mn = classManager.loadMethod(currentSig).get

        mn.instructions.iterator().asScala.foreach{
          case current: InvokeDynamicInsnNode =>
            if (Set(Util.metafactory, Util.altMetafactory).contains(SSA.InvokeDynamic.bootstrapFromHandle(current.bsm))){
              val target = current.bsmArgs(1).asInstanceOf[Handle]
              val targetSig = MethodSig(
                JType.Cls(target.getOwner),
                target.getName,
                Desc.read(target.getDesc),
                target.getTag == Opcodes.H_INVOKESTATIC
              )
              queue.enqueue(targetSig)
            }

          case current: MethodInsnNode if !ignore(current.owner) =>
            addSeenClassAndClInit(current.owner)
            val sig = MethodSig(
              current.owner, current.name,
              Desc.read(current.desc), current.getOpcode == Opcodes.INVOKESTATIC
            )
            if (current.getOpcode == Opcodes.INVOKEVIRTUAL ||
                current.getOpcode == Opcodes.INVOKEINTERFACE) {
              virtualCallSignatures(sig.cls) = sig :: virtualCallSignatures.getOrElse(sig.cls, Nil)
            }

            val possibleSigs = classManager.resolvePossibleSigs(sig).getOrElse(Nil)

            possibleSigs.foreach(sig => seenClasses.add(sig.cls))

            queue.enqueue(possibleSigs.filter(classManager.loadMethod(_).nonEmpty):_*)
            sig.desc.ret match{
              case cls: JType.Cls => seenClasses.add(cls)
              case _ => // do nothing
            }

          case current: FieldInsnNode => addSeenClassAndClInit(current.owner)

          case current: TypeInsnNode if current.getOpcode == Opcodes.NEW =>
            for(clsNode <- classManager.loadClass(current.desc)) {
              val supers = classManager.getAllSupertypes(current.desc)
              val methodNameDescs = clsNode
                .methods
                .asScala
                .map { m => (m.name, Desc.read(m.desc)) }.toSet

              for {
                sup <- supers
                virtualCallSig <- virtualCallSignatures.getOrElse(sup, Nil)
                if methodNameDescs((virtualCallSig.name, virtualCallSig.desc))
              } queue.enqueue(MethodSig(current.desc, virtualCallSig.name, virtualCallSig.desc, false))

            }


          // For ANEWARRAY and MULTIANEWARRAY, we only add the classes to seenClasses
          // but do not trigger <clinit> walking, the JVM just requires the class be
          // present but does not yet initialize it.
          case current: TypeInsnNode if current.getOpcode == Opcodes.ANEWARRAY =>
            unwrapArray(JType.read(current.desc), classManager).foreach(seenClasses.add)
          case current: MultiANewArrayInsnNode if current.getOpcode == Opcodes.MULTIANEWARRAY =>
            unwrapArray(JType.read(current.desc), classManager).foreach(seenClasses.add)

          case current: TypeInsnNode => seenClasses.add(JType.Cls(current.desc))

          case _ => // do nothing
        }
      }
    }

    val seenClassNames = (seenMethods.map(_.cls) ++ seenClasses).map(_.name).toSet
    val seenMethodNames = seenMethods
      .groupBy(_.cls.name)
      .mapValues(_.map(sig => (sig.name, sig.desc.unparse)).toSet)
      .toMap

    val seenClassNodes = classNodes.filter(cn => seenClassNames(cn.name))
    val visitedInterfaces = Util.findSeenInterfaces(classManager.loadClass, seenClassNodes.toSeq)
    val finalClassNodes = classNodes.filter(cn => seenClassNames(cn.name) || visitedInterfaces(cn.name))
    for (cn <- finalClassNodes){
      cn.methods.removeIf(m => !seenMethodNames.get(cn.name).fold(false)(_(m.name, m.desc)))
    }

    finalClassNodes
  }
  def unwrapArray(a: JType, classManager: ClassManager.Dynamic): Seq[JType.Cls] = {
    def rec(t: JType): Seq[JType.Cls] = t match{
      case arr: JType.Arr => rec(arr.innerType)
      case cls: JType.Cls => classManager.getAllSupertypes(cls)
      case _ => Nil
    }
    rec(a)
  }
}
