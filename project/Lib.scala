import sbt._
import Keys._
import com.jsuereth.sbtpgp.PgpKeys._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import xerial.sbt.Sonatype.autoImport._

object Lib {
  type PE = Project => Project

  private val verRegex = """^(\d+)\.(\d+)\.(\d+)-?(.+)?$""".r

  def byScalaVersion[A](f: PartialFunction[(Long, Long, Long, Option[String]), Seq[A]]): Def.Initialize[Seq[A]] =
    Def.setting(
      scalaVersion.value match {
        case verRegex(a, b, c, d) => f.lift((a.toInt, b.toInt, c.toInt, Option(d).filter(_.nonEmpty))).getOrElse(Nil)
        case _                    => Nil
      }
    )

  def addCommandAliases(m: (String, String)*): PE = {
    val s = m.map(p => addCommandAlias(p._1, p._2)).reduce(_ ++ _)
    _.settings(s: _*)
  }

  def sourceMapsToGithub(ghProject: String): Project => Project =
    p => p.settings(
      scalacOptions ++= {
        val isDotty = scalaVersion.value startsWith "3"
        val ver     = version.value
        if (isSnapshot.value)
          Nil
        else {
          val a = p.base.toURI.toString.replaceFirst("[^/]+/?$", "")
          val g = s"https://raw.githubusercontent.com/japgolly/$ghProject"
          val flag = if (isDotty) "-scalajs-mapSourceURI" else "-P:scalajs:mapSourceURI"
          s"$flag:$a->$g/v$ver/" :: Nil
        }
      }
    )

  def preventPublication: PE =
    _.settings(publish / skip := true)

  def addDirsFor213_+(scope: ConfigKey): Def.Initialize[Seq[File]] = Def.setting {
    (scope / unmanagedSourceDirectories).value.flatMap { dir =>
      if (dir.getPath.endsWith("scala"))
        CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, 12)) => Nil
          case _             => file(dir.getPath ++ "-2.13+") :: Nil
        }
      else
        Nil
    }
  }
}
