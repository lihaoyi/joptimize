package joptimize.frontend


import joptimize.{Logger, Util}
import joptimize.analyzer.Renderer
import joptimize.model.{InferredSig, MethodBody, MethodSig, SSA}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable

class Frontend(val classManager: ClassManager.Dynamic) {

  val cachedMethodBodies = mutable.LinkedHashMap.empty[InferredSig, Option[MethodBody]]
  def loadMethodBody(isig: InferredSig, log: Logger.Method): Option[MethodBody] = {
    cachedMethodBodies.getOrElseUpdate(isig, loadMethodBody0(isig.method, log))
  }

  def loadMethodBody0(originalSig: MethodSig, log: Logger.Method) = log.block {
    Util.labelExceptions(originalSig.toString) {
      val printer = new Textifier
      val methodPrinter = new TraceMethodVisitor(printer)
      val mn = classManager
        .loadMethod(originalSig)
        .getOrElse(throw new Exception("Unknown Sig: " + originalSig))
      if (mn.instructions.size() == 0) None
      else {
        log(Renderer.renderInsns(mn.instructions, printer, methodPrinter))

        val methodBody = ConstructSSA.apply(originalSig, mn, log)
        log.graph("RAW")(Renderer.dumpSvg(methodBody))
        log.check(methodBody.checkLinks(checkDead = false))
        methodBody.removeDeadNodes()
        log.graph("removeDeadNodes")(Renderer.dumpSvg(methodBody))
        log.check(methodBody.checkLinks())

        Frontend.simplifyPhiMerges(methodBody)
        log.graph("simplifyPhiMerges")(Renderer.dumpSvg(methodBody))
        log.check(methodBody.checkLinks())
        Some(methodBody)
      }
    }
  }

}
object Frontend {

  def simplifyPhiMerges(methodBody: MethodBody) = methodBody.transform {
    case n: SSA.InvokeSpecial if n.name == "<init>" && n.srcs(0).isInstanceOf[SSA.New0] =>
      val newNode = new SSA.New(n.cls, n.state, n.srcs.drop(1), n.desc)
      Util.replace(n, newNode)
      Util.replace(n.srcs(0), newNode)
    case phi: SSA.Phi =>
      val filteredValues = phi.incoming.filter(_._2 != phi)

      if (filteredValues.map(_._2).size == 1) {
        phi.block.phis = phi.block.phis.filter(_ != phi)
        Util.replace(phi, filteredValues.head._2)
      } else Nil

    case merge: SSA.Merge =>
      if (merge.incoming.size == 1 && merge.next != null) {
        for (next <- Option(merge.next) ++ merge.nextPhis) {
          next.replaceUpstream(merge, merge.incoming.head._1)
        }
        val nextState = merge.nextState
        val incomingState = merge.incoming.head._2
        nextState.parent = incomingState
        incomingState.next = nextState
        merge.incoming.head._1.next = merge.next
        merge.incoming.head._1.nextPhis = merge.nextPhis

        merge.next = null
        (merge.incoming.head._1 +: merge.phis) ++ merge.nextPhis ++ Option(merge.nextState)
      } else Nil
  }
}
