package mill.bspserver

import java.io._
import java.net.Socket

import scala.collection.JavaConverters._

import ammonite.ops._
import ammonite.runtime.SpecialClassLoader
import coursier.{Cache, CoursierPaths, Repository}

import mill.define._
import mill.eval.{Evaluator, PathRef, Result}
import mill.util.Ctx.{Home, Log}
import mill.util.Strict.Agg
import mill.util.{Loose, Strict}
import mill.{T, scalalib}
import mill.util.Logger
import scalalib.{CompilationResult, Lib, JavaModule, ScalaModule}


import io.circe.Json        // to construct Json values
import scala.meta.jsonrpc._ // for JSON-RPC APIs
// import scala.meta.lsp._     // for LSP endpoints

import ch.epfl.scala.bsp.endpoints.{BuildTarget => BuildTargetEndpoint, _}
import ch.epfl.scala.bsp._

class BspLogger(implicit client: LanguageClient) extends Logger {
  val colored = false

  val errorStream: PrintStream = new PrintStream(new OutputStream {
    def write(b: Int) = {}
  })
  val outputStream: PrintStream = errorStream
  val inStream: InputStream = new InputStream {
    def read() = ???
  }

  def debug(s: String): Unit = {}
  def info(s: String): Unit = {}
  def error(s: String): Unit = {}
  def ticker(s: String): Unit = {
    // TODO: Add a "Transient" severity to Diagnostic in the BSP ?
  }
  override def report(d: mill.util.Diagnostic): Unit = {
    // FIXME: BSP has no way to represent diagnostics without location information.
    // This should be supported since some error messages are not specific to a particular position
    if (d.location.isEmpty || d.location.get.range.isEmpty) {
      return
    }

    val loc = d.location.get
    val range = loc.range.get

    val startPos = Position(range.start.line, range.start.column)
    val endPos = Position(range.end.line, range.end.column)
    val bspRange = Range(startPos, endPos)
    val bspSeverity = Some({
      import mill.util.{DiagnosticSeverity => M}
      import ch.epfl.scala.bsp.{DiagnosticSeverity => B}
      d.severity match {
        case M.Error       => B.Error
        case M.Warning     => B.Warning
        case M.Information => B.Information
        case M.Hint        => B.Hint
      }
    })

    val bspDiag = Diagnostic(
      range = bspRange,
      severity = bspSeverity,
      code = d.code,
      source = None,
      message = d.message,
      relatedInformation = None
    )
    val pub = PublishDiagnosticsParams(
      textDocument = TextDocumentIdentifier(Uri(loc.path.toNIO.toUri.toString)),
      buildTarget = ???, // TODO !missing-implementation!
      originId = None,
      diagnostics = List(bspDiag)
    )
    Build.publishDiagnostics.notify(pub)
  }
}
