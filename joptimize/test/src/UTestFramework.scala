package test


class UTestFramework extends utest.runner.Framework {
  override def exceptionStackFrameHighlighter(s: StackTraceElement) = {
    s.getClassName.startsWith("joptimize.") || s.getClassName.startsWith("test.")
  }
  override def setup() = {
  }
}