package joptimize.frontend

import frontend.ConstructSSA
import joptimize.{FileLogger, Logger, Util}
import joptimize.analyzer.Renderer
import joptimize.model.{MethodSig, Program, SSA}
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

class Frontend {
  def apply(originalSig: MethodSig, mn: MethodNode, log: Logger.Method) = {
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    log.println("================ BYTECODE ================")
    log(Renderer.renderInsns(mn.instructions, printer, methodPrinter))

    val program = ConstructSSA.apply(originalSig.cls.name, mn, log)

    log.graph(Renderer.dumpSvg(program))
    program.removeDeadNodes()
    program.checkLinks()

    simplifyPhiMerges(program)
    program.checkLinks()
    program
  }


  def simplifyPhiMerges(program: Program) = program.transform{
    case phi: SSA.Phi =>
      val filteredValues = phi.incoming.filter(_._2 != phi)

      if (filteredValues.map(_._2).size == 1) Util.replace(phi, filteredValues.head._2)
      else Nil

    case reg: SSA.Merge =>
      if (reg.incoming.size == 1) Util.replace(reg, reg.incoming.head)
      else Nil
  }
}
