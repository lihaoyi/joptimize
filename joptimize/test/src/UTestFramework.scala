package joptimize


class UTestFramework extends utest.runner.Framework {
  override def exceptionStackFrameHighlighter(s: StackTraceElement) = {
    s.getClassName.startsWith("joptimize.")
  }
  override def setup() = {
    val original = os.pwd / 'out / 'original
    os.remove.all(original)
    os.remove.all(os.pwd / 'out / 'scratch)
    os.copy(
      os.pwd / 'out / 'joptimize / 'test / 'compile / 'dest / 'classes / 'joptimize / 'examples,
      original
    )
  }
}