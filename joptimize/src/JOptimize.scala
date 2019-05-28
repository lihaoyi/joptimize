package joptimize

import backend.{Backend, BytecodeDCE, Inliner}
import joptimize.analyzer.ProgramAnalyzer
import joptimize.frontend.{ClassManager, Frontend}
import joptimize.model._
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.{ClassVisitor, ClassWriter, Opcodes}
object JOptimize{
  def run(getClassFile: String => Option[Array[Byte]],
          entrypoints: Seq[MethodSig],
          log: Logger.Global,
          inline: Boolean): Map[String, Array[Byte]] = {

    val (analyzerRes, frozenClassManager) = new ProgramAnalyzer(
      entrypoints,
      log,
      new Frontend(
        new joptimize.frontend.ClassManager.Dynamic(
          name => ClassManager.loadClassNode(getClassFile(name + ".class"))
        )
      )
    ).apply()

    val outClasses = Backend.apply(
      if (inline) Inliner.inlineAll(analyzerRes, frozenClassManager, log)
      else analyzerRes,
      entrypoints,
      frozenClassManager,
      log,
    )

    val remaining = new BytecodeDCE(
      entrypoints,
      outClasses,
      ignore = _.startsWith("java/"),
      log = log
    ).apply()
    serialize(log, remaining)
//    serialize(log, outClasses)
  }

  def serialize(log: Logger.Global, outClasses: Seq[ClassNode]) = log.block {
    outClasses
      .map { cn =>
        if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaSig")
        if (cn.attrs != null) Util.removeFromJavaList(cn.attrs)(_.`type` == "ScalaInlineInfo")
        if (cn.visibleAnnotations != null) {
          Util.removeFromJavaList(cn.visibleAnnotations)(_.desc == "Lscala/reflect/ScalaSignature;")
        }

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
