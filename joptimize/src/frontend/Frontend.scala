package joptimize.frontend

import frontend.ConstructSSA
import joptimize.{FileLogger, Logger, Util}
import joptimize.analyzer.Renderer
import joptimize.model.{JType, MethodSig, Program, SSA}
import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

class Frontend(originalMethods: MethodSig => MethodNode,
               classExists0: JType.Cls => Boolean) {
  def classExists(cls: JType.Cls): Boolean = classExists0(cls)
  def apply(originalSig: MethodSig, log: Logger.Method): Option[Program] = {
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    val mn = originalMethods(originalSig)
    if(mn.instructions.size() == 0) None
    else {
      log.println("================ BYTECODE ================")
      log(Renderer.renderInsns(mn.instructions, printer, methodPrinter))

      val program = ConstructSSA.apply(originalSig, mn, log)

      log.graph(Renderer.dumpSvg(program))
      log.check(program.checkLinks(checkDead = false))
      program.removeDeadNodes()
      log.check(program.checkLinks())
      log.graph(Renderer.dumpSvg(program))

      simplifyPhiMerges(program)
      log.graph(Renderer.dumpSvg(program))
      log.check(program.checkLinks())
      Some(program)
    }
  }


  def simplifyPhiMerges(program: Program) = program.transform{
    case phi: SSA.Phi =>
      val filteredValues = phi.incoming.filter(_._2 != phi)

      if (filteredValues.map(_._2).size == 1) {
        phi.block.phis = phi.block.phis.filter(_ != phi)
        Util.replace(phi, filteredValues.head._2)
      }
      else Nil

    case merge: SSA.Merge =>
      if (merge.incoming.size == 1 && merge.next != null) {
        for(next <- Option(merge.next) ++ merge.nextPhis){

          next.replaceUpstream(merge, merge.incoming.head)
        }

        merge.incoming.head.next = merge.next
        merge.incoming.head.nextPhis = merge.nextPhis
        merge.next = null
        (merge.incoming.head +: merge.phis) ++ merge.nextPhis
      }
      else Nil
  }
}
