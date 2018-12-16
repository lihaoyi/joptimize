package joptimize

import java.io.PrintWriter
import java.io.StringWriter

import org.objectweb.asm.tree.AbstractInsnNode
import org.objectweb.asm.util.{Textifier, TraceMethodVisitor}

object Util{
  private val printer = new Textifier
  private val methodPrinter = new TraceMethodVisitor(printer)

  def prettyprint(insnNode: AbstractInsnNode) = {
    insnNode.accept(methodPrinter)
    val sw = new StringWriter
    printer.print(new PrintWriter(sw))
    printer.getText.clear
    sw.toString.stripSuffix("\n")
  }
  def mangle(name: String, stackTypes: Seq[JType], narrowReturnType: JType) = {
    val mangledName = name + "__" + stackTypes.mkString("__").replace('/', '_').replace(';', '_')
    val mangledDesc = Desc(stackTypes, narrowReturnType)
    (mangledName, mangledDesc)
  }
}
