package org.scalaide.play2

import scala.collection.immutable.Seq

trait Play {
  def templateImports: TemplateImports
}

trait TemplateImports {
  def defaultScalaTemplateImports: Seq[String]
  def defaultJavaTemplateImports: Seq[String]
  def templateResultType: String
  def templateFormatterType: String
}

object Play {
  final val SupportedVersion = List("2.4", "2.3", "2.2")
  final val DefaultVersion = SupportedVersion.head

  def apply(version: String): Play = version match {
    case "2.4" | "2.3" => new Play24
    case _             => ???
  }

  private final class Play24 extends Play {
    lazy val templateImports: TemplateImports = new Play24.Play24TemplateImports
  }

  private object Play24 {
    private final class Play24TemplateImports extends TemplateImports {
      // all imports have been copied from play.TemplateImports (see the playframework codebase)

      private def defaultTemplateImports: Seq[String] = List(
        "models._",
        "controllers._",
        "play.api.i18n._",
        "play.api.mvc._",
        "views.%format%._",
        "play.api.templates.PlayMagic._"
      )

      lazy val defaultJavaTemplateImports: Seq[String] = defaultTemplateImports ++ List(
        "java.lang._",
        "java.util._",
        "scala.collection.JavaConversions._",
        "scala.collection.JavaConverters._",
        "play.core.j.PlayMagicForJava._",
        "play.mvc._",
        "play.data._",
        "play.api.data.Field",
        "play.mvc.Http.Context.Implicit._"
      )

      lazy val defaultScalaTemplateImports: Seq[String] = defaultTemplateImports ++ List(
        "play.api.mvc._",
        "play.api.data._"
      )

      def templateResultType: String = templateFormatterType + ".Appendable"

      def templateFormatterType: String = "play.twirl.api.HtmlFormat"
    }
  }
}