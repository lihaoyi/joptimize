package joptimize

import joptimize.model.{Desc, JType, MethodSig}

object JOptimizeMain{
  def main(args: Array[String]): Unit = {
    val src = os.Path(args(0), os.pwd)
    val dest = os.Path(args(1), os.pwd)
    val entrypoints = for(arg <- args.drop(2)) yield {
      val (clsName, methodSigStr0) = arg.splitAt(arg.lastIndexOf('.'))
      val methodSigStr = methodSigStr0.drop(1)
      val (methodName, descStr) = methodSigStr.splitAt(methodSigStr.indexOf('('))
      val cls = JType.Cls(clsName)
      MethodSig(cls, methodName, Desc.read(descStr), static = true)
    }

    def loadIfExists(name: String): Option[Array[Byte]] = {
      val p = src / os.RelPath(name)
      if (os.exists(p)) Some(os.read.bytes(p))
      else None
    }

    val outputFileMap = joptimize.JOptimize.run(
      loadIfExists,
      entrypoints,
      log = joptimize.DummyLogger,
      inline = true
    )
    os.remove.all(dest)
    os.makeDir.all(dest)
    for ((k, bytes) <- outputFileMap) {
      os.write(dest / os.RelPath(k), bytes, createFolders = true)
    }

  }
}