package joptimize

import org.objectweb.asm.{ClassReader, ClassVisitor, MethodVisitor, Opcodes}
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.tree.analysis.Analyzer

import collection.JavaConverters._
object JOptimize{
  def run(classFiles: Map[String, Array[Byte]],
          entrypoints: Seq[(String, String)]) = {
    pprint.log(entrypoints)
    val classNodeMap = for((k, v) <- classFiles) yield {
      val cr = new ClassReader(v)
      val cn = new ClassNode()
      cr.accept(cn, ClassReader.SKIP_FRAMES)
      (k, cn)
    }


    for((clsName, methodName) <- entrypoints){
      pprint.log(classNodeMap.keys)
      val cn = classNodeMap(clsName)
      val mn = cn.methods.asScala.find(_.name == methodName).get
      pprint.log(mn)

      val df = new Dataflow()
      val analyzer = new Analyzer(df)
      analyzer.analyze(cn.name, mn)
      val frames = analyzer.getFrames
      pprint.log(frames)

    }
  }
}
