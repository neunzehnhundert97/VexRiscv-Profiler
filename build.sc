import mill._, scalalib._

import $ivy.`com.lihaoyi::mill-contrib-bloop:0.9.7`

object Profiler extends ScalaModule {
  def scalaVersion = "3.0.0"

  def scalacOptions =
    Seq("-explain", "-explain-types", "-no-indent", "-old-syntax")

  // Libraries
  def ivyDeps = Agg(
    ivy"com.github.pathikrit::better-files::3.9.1".withDottyCompat(scalaVersion()),
    ivy"com.lihaoyi::scalatags:0.9.4".withDottyCompat(scalaVersion()),
    ivy"com.lihaoyi::os-lib:0.7.8",
    ivy"com.lihaoyi::upickle::1.3.15",
    ivy"dev.zio::zio:1.0.9"
  )
}
