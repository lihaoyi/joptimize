package joptimize

import backend.Backend
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

    val classFileMap = for((k, v) <- classFiles) yield {
      val cr = new ClassReader(v)
      val cn = new ClassNode()
      cr.accept(cn, ClassReader.SKIP_FRAMES)
      (k, cn)
    }

    val classNodeMap = classFileMap.map{case (k, v) => (JType.Cls(v.name), v)}
    val subtypeMap = {
      val map = mutable.LinkedHashMap.empty[JType.Cls, List[JType.Cls]]
      for{
        (k, v) <- classNodeMap
        sup <- v.interfaces.asScala ++ Option(v.superName)
      } map(sup) = v.name :: map.getOrElse(sup, Nil)
      map
    }

    val originalMethods = for{
      (k, cls) <- classNodeMap
      m <- cls.methods.iterator().asScala
    } yield (MethodSig(cls.name, m.name, Desc.read(m.desc), (m.access & Opcodes.ACC_STATIC) != 0), m)

    def leastUpperBound(classes: Seq[JType.Cls]) = {
      Util.leastUpperBound(classes.toSet) { cls =>
        classNodeMap.get(cls) match{
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


    val resolver = new Analyzer.Resolver(
      classNodeMap,
      originalMethods,
      subtypeMap,
      leastUpperBound,
      merge,
    )
    val frontend = new Frontend(originalMethods, classNodeMap.contains)
    val (visitedMethods, visitedClasses) = Analyzer.apply(
      resolver,
      entrypoints,
      merge,
      log,
      frontend
    )

    log.pprint(visitedMethods)

    val outClasses = Backend.apply(
      resolver,
      entrypoints,
      originalMethods,
      classNodeMap,
      visitedMethods,
      eliminateOldMethods,
      classFileMap,
      visitedClasses,
      subtypeMap,
      log,
      leastUpperBound,
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
