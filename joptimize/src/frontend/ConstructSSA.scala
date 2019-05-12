package frontend

import joptimize.{FileLogger, Logger, Util}
import joptimize.analyzer.Renderer
import joptimize.frontend.{BytecodeToSSAInterpreter, ControlFlowExtraction}
import joptimize.model.{MethodSig, SSA}
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable
import collection.JavaConverters._
object ConstructSSA {

  def apply(sig: MethodSig,
            mn: MethodNode,
            log: Logger.Method) = {
    val phiMerges0 = mutable.LinkedHashSet.empty[SSA.Phi]

    val insns = mn.instructions.iterator().asScala.toVector
    val insnIndices = insns.zipWithIndex.toMap

    val regionStarts = ControlFlowExtraction.findRegionStarts(insns)
    val decoration = insns.zip(regionStarts).toMap
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)

    log(Renderer.renderInsns(mn.instructions, printer, methodPrinter, decorate = i => " " + pprint.apply(decoration(i))))
    val startRegionLookup = ControlFlowExtraction.findStartRegionLookup(insns, regionStarts)

    val argMapping = Util.argMapping(sig, _ => true).map(_.swap)
    val program = ControlFlowExtraction.extractControlFlow(
      insns,
      i => regionStarts(insnIndices(i)),
      joptimize.frontend.DataflowExecutor.analyze(
        sig.cls.name, mn, regionStarts,
        new BytecodeToSSAInterpreter(phiMerges0, startRegionLookup, regionStarts, argMapping),
        new SSA.ChangedState(regionStarts(0).get)
      ),
      startRegionLookup
    )

    program
  }
}
