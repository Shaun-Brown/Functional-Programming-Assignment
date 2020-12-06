/*
 * CESIL+ pretty printer
 * Author: David Smallwood
 * January 2020
 */

package cesil

import cesil.Lang._

/**
 * A library of methods to output CESIL+ programs in a human-readable, vertical form.
 */
object Pretty {
  
  private def formatUnlabelled(unlabelled: Unlabelled): (String, String) = unlabelled match {
    case LOAD(variable: Var)     => ("LOAD", variable.varid.toString)
    case LOAD(value: Val)        => ("LOAD", value.literal.toString)
    case STORE(variable: Var)    => ("STORE", variable.varid.toString)
    case JUMP(label)             => ("JUMP", label.toString)
    case JINEG(label)            => ("JINEG", label.toString)
    case JIPOS(label)            => ("JIPOS", label.toString)
    case JIZERO(label)           => ("JIZERO", label.toString)
    case PRINT(string)           => ("PRINT", "\"" + string + "\"")
    case LINE                    => ("LINE", "")
    case IN                      => ("IN", "")
    case OUT                     => ("OUT", "")
    case ADD(variable: Var)      => ("ADD", variable.varid.toString)
    case ADD(value: Val)         => ("ADD", value.literal.toString)
    case SUBTRACT(variable: Var) => ("SUBTRACT", variable.varid.toString)
    case SUBTRACT(value: Val)    => ("SUBTRACT", value.literal.toString)
    case MULTIPLY(variable: Var) => ("MULTIPLY", variable.varid.toString)
    case MULTIPLY(value: Val)    => ("MULTIPLY", value.literal.toString)
    case DIVIDE(variable: Var)   => ("DIVIDE", variable.varid.toString)
    case DIVIDE(value: Val)      => ("DIVIDE", value.literal.toString)
    case MODULO(variable: Var)   => ("MODULO", variable.varid.toString)
    case MODULO(value: Val)      => ("MODULO", value.literal.toString)
    case PUSH                    => ("PUSH", "")
    case POP                     => ("POP", "")
    case SWAP                    => ("SWAP", "")
    case NOP                     => ("NOP", "")
    case HALT                    => ("HALT", "")
  }

  private def formatStatement(statement: Statement): (String, (String, String)) = statement match {
    case Labelled(label, unlabelled) => (label.toString, formatUnlabelled(unlabelled))
    case unlabelled: Unlabelled      => ("", formatUnlabelled(unlabelled))
  }

  private def pretty(line: (String, (String, String))): String = line match {
    case (label, (operator, operand)) => f"$label%-8s$operator%-10s$operand"
  }

  /**
   * Converts a CESIL program into a list of strings. Each string is a textual representation
   * of the respective command in the program. It is provided so that the caller can format
   * each line of the program using some other external text formatting application.
   */
  def pretty(statements: Statements): List[String] =
    statements.map(formatStatement).map(pretty)

  /**
   * Converts a CESIL program into a string for conventional (vertical) display. The optional
   * parameter `displayWithLineNumbers` can be set to `false` to suppress the line numbers
   * being printed as well. If the optional parameter is omitted then line numbers appear by
   * default. The result is simply a string designed to be output using, e.g., `println()`.
   */
  def verticalFormat(statements: Statements, displayWithLineNumbers: Boolean = true): String = {
    val formattedLines = pretty(statements)

    def showIndexedLine(indexedLine: (Int, String)): String = f"${indexedLine._1}%4d | ${indexedLine._2}"

    (if (displayWithLineNumbers)
       (formattedLines.indices zip formattedLines).map(showIndexedLine)
     else
       formattedLines).mkString("\n")
  }
}
