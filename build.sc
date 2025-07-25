import mill._
import mill.scalalib._
import mill.scalajslib._
import mill.scalajslib.api.ModuleKind

object joptimize extends ScalaModule{
  def moduleDeps = Seq(viewer.model)
  def scalaVersion = "2.13.0"
  def ivyDeps = Agg(
    ivy"org.ow2.asm:asm:7.0",
    ivy"org.ow2.asm:asm-tree:7.0",
    ivy"org.ow2.asm:asm-analysis:7.0",
    ivy"org.ow2.asm:asm-commons:7.0",
    ivy"org.ow2.asm:asm-util:7.0",
    ivy"com.lihaoyi::os-lib:0.3.0",
    ivy"com.lihaoyi::pprint:0.5.5"
  )
  def compileIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  def scalacOptions = Seq("-P:acyclic:force")
  def scalacPluginIvyDeps = Agg(ivy"com.lihaoyi::acyclic:0.2.0")
  object test extends Tests{
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.1",
      ivy"com.lihaoyi::ujson:0.7.5"
    )
    def testFrameworks = Seq("test.UTestFramework")

    def unzippedScalaFolder = T{
      val zipFile = new java.util.zip.ZipFile(
        compileClasspath().find(_.path.toString.contains("scala-library")).get.path.toString
      )
      import collection.JavaConverters._
      for(x <- zipFile.entries.asScala){
        if (!x.isDirectory){
          os.write(
            T.ctx().dest / os.RelPath(x.getName),
            zipFile.getInputStream(x),
            createFolders = true
          )
        }
      }

      PathRef(T.ctx().dest)
    }

    def classScalaFolder = T{
      for(base <- Seq(compile().classes.path, unzippedScalaFolder().path)){
        for(sub <- os.list(base)){
          os.copy(sub, T.ctx().dest / sub.relativeTo(base))
        }
      }
      PathRef(T.ctx().dest)
    }
    def forkEnv = Map(
      "CLASSES_FOLDER" -> classScalaFolder().path.toString
    )
    def scalacOptions = super.scalacOptions() ++ Seq("-Ydelambdafy:inline")
  }
}

object viewer extends ScalaModule{
  def moduleDeps = Seq(viewer.model)
  def scalaVersion = "2.13.0"
  def ivyDeps = Agg(
    ivy"com.lihaoyi::cask:0.2.1",
    ivy"com.lihaoyi::os-lib:0.3.0",
    ivy"com.lihaoyi::scalatags:0.7.0",
    ivy"guru.nidi:graphviz-java:0.8.3",
  )

  object model extends ScalaModule{
    def scalaVersion = "2.13.0"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::upickle:0.7.5",
      ivy"com.lihaoyi::pprint:0.5.5"
    )
  }
}
