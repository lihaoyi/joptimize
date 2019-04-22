package joptimize

import backend.Backend
import joptimize.analyzer.Analyzer
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.model._
import org.objectweb.asm.{ClassVisitor, ClassWriter, Opcodes}
object JOptimize{
  def run(classFiles: Map[String, Array[Byte]],
          entrypoints: Seq[MethodSig],
          eliminateOldMethods: Boolean,
          log: Logger.Global): Map[String, Array[Byte]] = {

    val classManager = new ClassManager(classFiles.get)
    val frontend = new Frontend(classManager)

    var analyzerRes: Analyzer.GlobalResult = null
//    while({
//      val initialState = classManager.loadClassCache.keysIterator.toSet
      val analyzer = new Analyzer(entrypoints, classManager, log, frontend)
      analyzerRes = analyzer.apply()
//      val finalState = classManager.loadClassCache.keysIterator.toSet
//      finalState != initialState
//    })()

//    log.pprint(analyzerRes.visitedMethods)

//    pprint.log(analyzerRes.visitedClasses)
    val outClasses = Backend.apply(
      analyzerRes,
      entrypoints,
      classManager,
      eliminateOldMethods,
      log,
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
