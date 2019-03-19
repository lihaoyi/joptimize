package joptimize.analysis

import joptimize.Util
import joptimize.model.{Desc, JType, MethodSig, SSA}
import org.objectweb.asm.tree.InvokeDynamicInsnNode
import org.objectweb.asm.{Handle, Opcodes}
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
            findSupertypes: JType.Cls => Seq[JType.Cls],
            ignore: String => Boolean): Seq[ClassNode] = {

    val classNodeMap = classNodes.map{cn => (JType.Cls(cn.name), cn)}.toMap
    val allMethodSigs = for{
      cn <- classNodes
      mn <- cn.methods.iterator().asScala
    } yield (MethodSig(cn.name, mn.name, Desc.read(mn.desc), (mn.access & Opcodes.ACC_STATIC) != 0), mn)
    val methodSigMap = allMethodSigs.toMap
    pprint.log(methodSigMap.mapValues(_.name))

    val queue = entrypoints.to[mutable.Queue]
    val seenMethods = mutable.LinkedHashSet.empty[MethodSig]
    val seenClasses = mutable.LinkedHashSet.empty[JType.Cls]

    while(queue.nonEmpty){
      val current = queue.dequeue()
      if (!ignore(current.cls.name) && !seenMethods(current)){
        seenMethods.add(current)
        seenClasses.add(current.cls)
        pprint.log(current)
        val mn = methodSigMap(
          if (!current.static) current
          else {
            val x =
              findSupertypes(current.cls)
            pprint.log(x)
            val y = x
              .iterator
              .map(MethodSig(_, current.name, current.desc, current.static))
              .filter(methodSigMap.contains)

            pprint.log(y.hasNext)

            y
              .take(1)
              .toSeq
              .head
          }
        )

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

            val sig = MethodSig(
              current.owner, current.name,
              Desc.read(current.desc), current.getOpcode == Opcodes.INVOKESTATIC
            )

            val subtypes = if (sig.static) findSupertypes(sig.cls) else findSubtypes(sig.cls)
            val possibleSigs = subtypes.map(st => sig.copy(cls = st)) ++ Seq(sig)

            queue.enqueue(possibleSigs.filter(methodSigMap.contains):_*)

          case current: FieldInsnNode =>
            val clinitMethod = MethodSig(current.owner, "<clinit>", Desc.read("()V"), true)
            if (methodSigMap.contains(clinitMethod)) queue.enqueue(clinitMethod)
            seenClasses.add(current.owner)

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
