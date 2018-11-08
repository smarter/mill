package mill.scalalib.worker


import java.io.File
import java.util.Optional

import ammonite.ops.{Path, exists, ls, mkdir}
import ammonite.util.Colors
import mill.Agg
import mill.eval.PathRef
import mill.scalalib.{CompilationResult, Lib, TestRunner}
import xsbti.compile.{CompilerCache => _, FileAnalysisStore => _, ScalaInstance => _, _}
import mill.scalalib.Dep.isDotty
import mill.scalalib.Lib.{grepJar, scalaBinaryVersion}
import mill.util.{Ctx, Diagnostic, Location, Position, PrintLogger, Range}
import sbt.internal.inc._
import sbt.internal.util.{ConsoleOut, MainAppender}
import sbt.util.LogExchange

import scala.collection.mutable
class MillReporter(logger: mill.util.Logger)
  extends xsbti.Reporter {

  private[this] var errorCount = 0
  private[this] var warningCount = 0
  private[this] val allProblems = new mutable.ArrayBuffer[xsbti.Problem]

  override def reset(): Unit = {
    errorCount = 0
    warningCount = 0
  }

  override def hasErrors: Boolean = errorCount > 0
  override def hasWarnings: Boolean = warningCount > 0

  override def printSummary: Unit = {
    if (warningCount > 0)
      logger.info(s"$warningCount warning(s) found")
    if (errorCount > 0)
      logger.error(s"$errorCount error(s) found")
  }

  override def problems: Array[xsbti.Problem] = allProblems.toArray

  override def log(problem: xsbti.Problem): Unit = {
    val sbtPos = problem.position
    val path = toOption(sbtPos.sourcePath).map(Path(_))

    val pos = toOption(sbtPos.line).flatMap(l =>
      toOption(sbtPos.pointer).map(c => Position(l, c)))
    // Zinc does not record the start/end of a message currently, just a point.
    val range = pos.map(p => Range(p, p))
    val location = path.map(Location(_, range))

    val severity = {
      import xsbti.{Severity => S}
      import mill.util.{DiagnosticSeverity => DS}
      problem.severity match {
        case S.Error => DS.Error
        case S.Warn  => DS.Warning
        case S.Info  => DS.Information
      }
    }

    // Zinc does not record diagnostic codes currently.
    val code = None

    val diagnostic = Diagnostic(location, severity, code, problem.message)

    logger.report(diagnostic)
  }

  // TODO: what is this for ?
  override def comment(pos: xsbti.Position, msg: String): Unit = {}

  private def toOption[T](x: Optional[T]): Option[T] =
    if (x.isPresent) Some(x.get) else None
}
