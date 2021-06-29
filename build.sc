import mill._, scalalib._

import $ivy.`com.lihaoyi::mill-contrib-bloop:0.9.7`

object Profiler extends ScalaModule {
  def scalaVersion = "3.0.0"

  def scalacOptions =
    Seq("-explain", "-explain-types", "-source:3.0-migration", "-no-indent", "-old-syntax")

  // Compiler plugins
  //def scalacPluginIvyDeps = Agg()

  // Libraries
  def ivyDeps = Agg(
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.3",
    ivy"com.github.pathikrit::better-files::3.9.1".withDottyCompat(scalaVersion()),
    ivy"com.lihaoyi::scalatags:0.9.4".withDottyCompat(scalaVersion())
  )

  // Arguments for java
  //def forkArgs = Seq()

}
