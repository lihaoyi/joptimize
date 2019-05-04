package joptimize.frontend

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}

import frontend.ConstructSSA
import joptimize.algorithms.MultiBiMap
import joptimize.{FileLogger, Logger, Util}
import joptimize.analyzer.Renderer
import joptimize.model.{IType, InferredSig, JType, MethodBody, MethodSig, SSA}
import org.objectweb.asm.tree.{ClassNode, MethodNode}
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

import scala.collection.mutable

class Frontend(val classManager: ClassManager) {

  val cachedMethodBodies = mutable.LinkedHashMap.empty[InferredSig, Option[MethodBody]]
  def loadMethodBody(isig: InferredSig, log: Logger.Method): Option[MethodBody] = {
    cachedMethodBodies.getOrElseUpdate(isig, loadMethodBody0(isig.method, log))
  }

  def loadMethodBody0(originalSig: MethodSig, log: Logger.Method) = {
    val printer = new Textifier
    val methodPrinter = new TraceMethodVisitor(printer)
    val mn = classManager.loadMethod(originalSig).getOrElse(throw new Exception("Unknown Sig: " + originalSig))
    if(mn.instructions.size() == 0) None
    else {
      log.println("================ BYTECODE ================")
      log(Renderer.renderInsns(mn.instructions, printer, methodPrinter))

      val methodBody = ConstructSSA.apply(originalSig, mn, log)

      log.graph(Renderer.dumpSvg(methodBody))
      log.check(methodBody.checkLinks(checkDead = false))
      methodBody.removeDeadNodes()
      log.graph(Renderer.dumpSvg(methodBody))
      log.check(methodBody.checkLinks())
      log.graph(Renderer.dumpSvg(methodBody))

      simplifyPhiMerges(methodBody)
      log.graph(Renderer.dumpSvg(methodBody))
      log.check(methodBody.checkLinks())
      Some(methodBody)
    }
  }


  def simplifyPhiMerges(methodBody: MethodBody) = methodBody.transform{
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
