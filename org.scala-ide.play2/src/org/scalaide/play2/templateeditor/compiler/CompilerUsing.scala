package org.scalaide.play2.templateeditor.compiler

import java.io.File
import scala.util.Failure
import scala.util.Try
import org.eclipse.jdt.core.compiler.IProblem
import org.eclipse.jdt.internal.compiler.problem.DefaultProblem
import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities
import org.scalaide.logging.HasLogger
import org.scalaide.play2.PlayProject
import play.twirl.compiler.GeneratedSourceVirtual
import play.twirl.compiler.TwirlCompiler
import play.twirl.compiler.TemplateCompilationError
import org.scalaide.play2.properties.PlayPreferences
import org.scalaide.logging.HasLogger
import scala.io.Codec
import org.scalaide.core.compiler.ScalaCompilationProblem
/**
 * a helper for using template compiler
 */
object CompilerUsing extends HasLogger {
  private val templateCompiler = TwirlCompiler

  /**
   * invokes compile method of template compiler and returns generated source object or
   * in the case of error, returns appropriate exception
   */
  def compileTemplateToScalaVirtual(content: String, source: File, playProject: PlayProject, inclusiveDot: Boolean): Try[GeneratedSourceVirtual] = {
    val sourcePath = playProject.sourceDir.getAbsolutePath()
    if (source.getAbsolutePath().indexOf(sourcePath) == -1)
      logger.debug(s"Template file '${source.getAbsolutePath}' must be located in '$sourcePath' or one of its subfolders!")

    val extension = source.getName.split('.').last
    val templateImports = playProject.playSupport.templateImports

    Try {
      templateCompiler.compileVirtual(
        content,
        source,
        playProject.sourceDir,
        templateImports.templateResultType,
        templateImports.templateFormatterType,
        formatImports(templateImports.defaultScalaTemplateImports, extension),
        // formatImports(playProject.additionalTemplateImports(extension), extension),
        Codec.default,
        inclusiveDot
      )
    } recoverWith {
      case TemplateCompilationError(source, message, line, column) =>
        val offset = PositionHelper.convertLineColumnToOffset(content, line, column)
        Failure(TemplateToScalaCompilationError(source, message, offset, line, column))
      case ex: Exception => {
        val error = s"Caught unknown exception: '${ex.getMessage()}'\n${ex.getStackTraceString}"
        Failure(TemplateToScalaCompilationError(source, error, 0, 0, 0))
      }
    }
  }

  private def formatImports(templateImports: Seq[String], extension: String): String = {
    templateImports.map("import " + _.replace("%format%", extension)).mkString("\n")
  }
}

case class TemplateToScalaCompilationError(source: File, message: String, offset: Int, line: Int, column: Int) extends RuntimeException(message) {
  override def toString = source.getName + ": " + message + offset + " " + line + "." + column

  import org.eclipse.jdt.internal.compiler.problem.ProblemSeverities

  def toProblem: ScalaCompilationProblem =
    ScalaCompilationProblem(
      source.getAbsolutePath().toString,
      ProblemSeverities.Error,
      message,
      Math.max(offset - 1, 0),
      Math.max(offset - 1, 0),
      line,
      column)
}

object PositionHelper {
  def convertLineColumnToOffset(source: File, line: Int, column: Int): Int = {
    convertLineColumnToOffset(scala.io.Source.fromFile(source).mkString, line, column)
  }

  def convertLineColumnToOffset(content: String, line: Int, column: Int): Int = {
    // splitting the string will cause some problems
    var offset = 0
    for (i <- 1 until line) {
      offset = content.indexOf("\n", offset) + 1
    }
    offset += column - 1
    offset
  }

  def mapSourcePosition(matrix: Seq[(Int, Int)], sourcePosition: Int): Int = {
    val sortedMatrix = matrix.sortBy(_._2)
    sortedMatrix.indexWhere(p => p._2 > sourcePosition) match {
      case 0 => 0
      case i if i > 0 => {
        val pos = sortedMatrix(i - 1)
        pos._1 + (sourcePosition - pos._2)
      }
      case _ => {
        val pos = sortedMatrix.takeRight(1)(0)
        pos._1 + (sourcePosition - pos._2)
      }
    }
  }
}
