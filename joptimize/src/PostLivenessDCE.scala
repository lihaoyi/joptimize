package joptimize

import org.objectweb.asm.Opcodes
import org.objectweb.asm.tree.{ClassNode, FieldInsnNode, MethodInsnNode, TypeInsnNode}

import collection.JavaConverters._
import scala.collection.mutable

/**
  * Post-liveness cleanup phase to remove any classes which have been rendered
  * unreachable by the liveness analysis. We cannot do this during the initial
  * walk because we need all liveness analysis to be complete before we can
  * scan for new unreachability.
  */
object PostLivenessDCE {
  def apply(entrypoints: scala.Seq[MethodSig],
            classNodes: Seq[ClassNode],
            findSubtypes: JType.Cls => List[JType.Cls],
            classNodeMap: Map[JType.Cls, ClassNode]): Seq[ClassNode] = {
    val queue = entrypoints.to[mutable.Queue]
    val seenMethods = mutable.Set.empty[MethodSig]
    val seenClasses = mutable.Set.empty[JType.Cls]
    while(queue.nonEmpty){
      val current = queue.dequeue()
      if (!seenMethods(current)){
        seenMethods.add(current)
        val cn = classNodes.find(_.name == current.cls.name).getOrElse(throw new Exception(current.cls.name))
        val mn = cn.methods.iterator().asScala
          .find(mn => mn.name == current.name && mn.desc == current.desc.unparse)
          .get

        mn.instructions.iterator().asScala.foreach{
          case current: MethodInsnNode if !current.owner.startsWith("java/")=>
            val sig = MethodSig(
              current.owner, current.name,
              Desc.read(current.desc), current.getOpcode == Opcodes.INVOKESTATIC
            )

            val subtypes = findSubtypes(sig.cls)
            val possibleSigs = subtypes.map(st => sig.copy(cls = st)) ++ Seq(sig)

            queue.enqueue(possibleSigs:_*)

          case current: FieldInsnNode =>
            queue.enqueue(MethodSig(current.owner, "<clinit>", Desc.read("()V"), true))

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
    val visitedInterfaces = Util.findSeenInterfaces(classNodeMap, seenClassNodes.toList)
    val finalClassNodes = classNodes.filter(cn => seenClassNames(cn.name) || visitedInterfaces(cn.name))
    for (cn <- finalClassNodes){
      cn.methods.removeIf(m => !seenMethodNames.get(cn.name).fold(false)(_(m.name, m.desc)))
    }
    finalClassNodes
  }
}
