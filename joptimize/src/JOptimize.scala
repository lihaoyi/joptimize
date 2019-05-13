package joptimize

import backend.Backend
import joptimize.analyzer.ProgramAnalyzer
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.model._
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.{ClassVisitor, ClassWriter, Opcodes}
object JOptimize{
  def run(getClassFile: String => Option[Array[Byte]],
          entrypoints: Seq[MethodSig],
          eliminateOldMethods: Boolean,
          log: Logger.Global,
          inline: Boolean): Map[String, Array[Byte]] = {

    val classManager = new ClassManager(getClassFile)
    val frontend = new Frontend(classManager)

    val analyzer = new ProgramAnalyzer(entrypoints, classManager, log, frontend)
    val analyzerRes = analyzer.apply()

//    pprint.log(analyzerRes.visitedMethods)
//    pprint.log(analyzerRes.visitedResolved)
//    pprint.log(analyzerRes.visitedMethods.mapValues(_.inferred))
    val outClasses = Backend.apply(
      analyzerRes,
      entrypoints,
      classManager,
      eliminateOldMethods,
      log,
      inline
    )

    serialize(log, outClasses)
  }


  def serialize(log: Logger.Global, outClasses: Seq[ClassNode]) = log.block {
    outClasses
      .map { cn =>
        log.pprint(cn.name)
        val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        cn.accept(new ClassVisitor(Opcodes.ASM7, cw) {
          override def visitMethod(access: Int,
                                   name: String,
                                   descriptor: String,
                                   signature: String,
                                   exceptions: Array[String]) = {
            val ref = name + descriptor
            log.pprint(ref)
            super.visitMethod(access, name, descriptor, signature, exceptions)
          }
        })
        (cn.name + ".class", cw.toByteArray)
      }
      .toMap
  }
}
