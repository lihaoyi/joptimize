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

    val interp = new AbstractInterpret(
      s => (classNodeMap(s).access & Opcodes.ACC_INTERFACE) != 0,
      visited
    )

    for(entrypoint <- entrypoints){
      val mn = originalMethods(entrypoint)
      interp.interpretMethod(
        mn.instructions,
        Type.getType(mn.desc).getArgumentTypes.map(Inferred(_)).toList,
        mn.maxLocals,
        mn.maxStack
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
      val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
      cn.accept(cw)
      (k, cw.toByteArray)
    }
  }
}
