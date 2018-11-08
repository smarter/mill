package mill.util

import ammonite.ops.Path

/** Represents a diagnostic, such as a compiler error or warning.
 *
 *  @param location  The location of this message.
 *  @param severity  The diagnostic's severity.
 *  @param code      The diagnostic's code. Can be omitted.
 *  @param message   The diagnostic's message.
 */
case class Diagnostic(
  location: Option[Location],
  severity: DiagnosticSeverity,
  code: Option[String],
  message: String
)

sealed abstract class DiagnosticSeverity(val id: Int)
object DiagnosticSeverity {
  case object Error extends DiagnosticSeverity(1)
  case object Warning extends DiagnosticSeverity(2)
  case object Information extends DiagnosticSeverity(3)
  case object Hint extends DiagnosticSeverity(4)
}

case class Location(
  path: Path,
  range: Option[Range]
)

// FIXME: I'd rather have Range(startOffset: Int, endOffset: Int) but the BSP
// currently uses line/column
case class Range(
  start: Position,
  end: Position
)

case class Position(
  line: Int,
  column: Int
)
