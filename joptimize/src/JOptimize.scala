package joptimize

import backend.Backend
import joptimize.algorithms.MultiBiMap
import joptimize.analyzer.Analyzer
import joptimize.frontend.Frontend
import joptimize.model._
import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter, Opcodes}
import org.objectweb.asm.tree._

import collection.JavaConverters._
import scala.collection.mutable
object JOptimize{
  def run(classFiles: Map[String, Array[Byte]],
          entrypoints: Seq[MethodSig],
          eliminateOldMethods: Boolean,
          log: Logger.Global): Map[String, Array[Byte]] = {

    val subtypeMap = new MultiBiMap.Mutable[JType.Cls, JType.Cls]

    val loadClassCache = mutable.LinkedHashMap.empty[JType.Cls, Option[ClassNode]]
    def loadClass(cls: JType.Cls): Option[ClassNode] = loadClassCache.getOrElseUpdate(
      cls,
      classFiles.get(cls.name + ".class").map { file =>
        val cr = new ClassReader(file)
        val cn = new ClassNode()
        cr.accept(cn, ClassReader.SKIP_FRAMES)
        val uppers = cn.interfaces.asScala ++ Option(cn.superName)

        for(up <- uppers){
          subtypeMap.add(up, cls)
        }

        cn
      }
    )

    val loadMethodCache = mutable.LinkedHashMap.empty[MethodSig, Option[MethodNode]]
    def loadMethod(sig: MethodSig): Option[MethodNode] = loadMethodCache.getOrElseUpdate(
      sig,
      loadClass(sig.cls).flatMap { cls =>
        cls.methods.iterator().asScala.find( mn =>
          mn.name == sig.name && mn.desc == sig.desc.unparse
        )
      }
    )


    def leastUpperBound(classes: Seq[JType.Cls]) = {
      Util.leastUpperBound(classes.toSet) { cls =>
        loadClass(cls) match{
          case None => Nil
          case Some(cn) => (cn.interfaces.asScala ++ Option(cn.superName)).map(JType.Cls(_))
        }
      }.toSeq
    }

    def merge(itypes: Seq[IType]): IType = {
      val flattened = itypes.flatMap{
        case CType.Intersect(values) => values
        case j => Seq(j)
      }.distinct
      if (flattened.length == 1) flattened.head
      else if (flattened.forall(_.isRef)){
        if (flattened.exists(_.isInstanceOf[JType.Arr])) JType.Cls("java/lang/Object")
        else leastUpperBound(flattened.map(_.asInstanceOf[JType.Cls])) match{
          case Seq(single) => single
          case many => CType.Intersect(many)
        }
      }
      else if(flattened.forall(_.widen == JType.Prim.I)) JType.Prim.I
      else if(flattened.forall(_.widen == JType.Prim.F)) JType.Prim.F
      else if(flattened.forall(_.widen == JType.Prim.J)) JType.Prim.J
      else if(flattened.forall(_.widen == JType.Prim.D)) JType.Prim.D
      else if(flattened.forall(x => x.widen == JType.Prim.Z || x.getClass == classOf[CType.I])) JType.Prim.Z
      else if(flattened.forall(x => x.widen == JType.Prim.B || x.getClass == classOf[CType.I])) JType.Prim.B
      else if(flattened.forall(x => x.widen == JType.Prim.C || x.getClass == classOf[CType.I])) JType.Prim.C
      else if(flattened.forall(x => x.widen == JType.Prim.S || x.getClass == classOf[CType.I])) JType.Prim.S
      else throw new Exception(flattened.toString)
    }

    val frontend = new Frontend(loadMethod, loadClass, subtypeMap)

    val analyzer = new Analyzer(
      entrypoints,
      merge,
      log,
      frontend
    )

    val (visitedMethods, visitedResolved, visitedClasses) = analyzer.apply()

    log.pprint(visitedMethods)

    val outClasses = Backend.apply(
      visitedResolved,
      entrypoints,
      loadMethodCache.collect{case (k, Some(v)) => (k, v)}.toMap,
      loadClassCache.collect{case (k, Some(v)) => (k, v)}.toMap,
      visitedMethods,
      eliminateOldMethods,
      visitedClasses,
      subtypeMap,
      log,
      merge,
    )

    outClasses
      .map{cn =>
        log.pprint(cn.name)
        val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        cn.accept(new ClassVisitor(Opcodes.ASM7, cw) {
          override def visitMethod(access: Int,
                                   name: String,
                                   descriptor: String,
                                   signature: String,
                                   exceptions: Array[String]) = {
            log.pprint(access, name, descriptor, signature)
            super.visitMethod(access, name, descriptor, signature, exceptions)
          }
        })
        (cn.name + ".class", cw.toByteArray)
      }
      .toMap
  }


}
