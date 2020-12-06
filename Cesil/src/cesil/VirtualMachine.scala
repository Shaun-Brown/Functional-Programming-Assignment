/*
 * CESIL+ virtual machine
 * Author: David Smallwood
 * January 2020
 */

package cesil

import cesil.Lang._
import scala.collection.mutable
import Analyser._

/**
  * A virtual machine (VM) for executing CESIL programs. To use an instance of this class it is
  * neceessary to (i) load a program and then (ii) run a program. For example:
  *
  * @example {{{
  *           vm = new VirtualMachine()
  *           val prog = IN ++ ADD(Val(1)) ++ OUT ++ HALT
  *           vm.load(prog)
  *           val result = vm.run(List(3))
  *           println(result)
  * }}}
  * The program reads a value from the input stream (List(3)); adds 1; then outputs
  * the result (4). The result is a String which can be printed.
  *
  * The virtual machine consists of a stored program; a program counter; an accumulator;
  * a stack; a block of memory for storing variables; an input stream and an output stream.
  * We reserve labels beginning with a hash (#) symbol for our own use, and also the
  * reserved variable name #R. Otherwise, you are free to use any strings you like for
  * naming labels and variables but the convention would be to start them with a letter
  * and stick to alphanumeric characters.
  *
  * The VM operates on programs represented by (Scala) lists of [[Lang.Statements]]. There is
  * no parser so the VM only accepts the statements as Scala case classes (rather than strings).
  * For example, the instruction
  * {{{
  *   RPT  LOAD  x
  * }}}
  * is represented by `Labelled`(`RPT`, `LOAD`(`Var`("x"))).
  *
  */
class VirtualMachine {
  private var program     : Vector[Unlabelled]                = _
  private var acc         : NumLiteral                        = 0
  private var pc          : Int                               = 0
  private var end         : Int                               = 0
  private var labels      : Map[Label, Int]                   = _
  private var variables   : mutable.Map[VarIdent, NumLiteral] = _
  private var stack       : mutable.Stack[NumLiteral]         = _
  private var inputStream : Input                             = _
  private var outputStream: Output                            = ""
  private var stepCounter : Int                               = 0
  private var traceOn     : Boolean                           = true

  /**
    * @return the number of steps executed.
    */
  def getStepCounter: Int = stepCounter

  /**
    * @return the program counter.
    */
  def getPC: Int = pc

  /**
    * @return the value in the accumulator.
    */
  def getAcc: NumLiteral = acc

  /**
    * @return the mapping of variable (identifiers) to their currently stored values.
    */
  def getVariables: Map[VarIdent, NumLiteral] = variables.toMap

  /**
    * @return the mapping of labels to their respective line numbers.
    */
  def getLabels: Map[Label, NumLiteral] = labels

  /**
    * @return a copy of the stack.
    */
  def getStackAsList: List[NumLiteral] = stack.toList

  override def toString: String = {
    s"""step counter ($stepCounter), program counter ($pc), program length ($end), accumulator ($acc)
       |program: ${(program.indices zip program).toString()}
       |${"labels: %s".format(labels)}
       |variables: $variables
       |$stack
       |input stream  = $inputStream
       |output stream = "$outputStream"
       |----
       |""".stripMargin
  }

  /**
    * Loads a CESIL program into the VM.
    *
    * @param statements the program to load into the machine.
    */
  def load(statements: Statements): Unit = synchronized {
    labels = makeLabelIndexMap(statements)
    program = stripLabels(statements).toVector
    end = program.length
  }

  /**
    * Runs the currently loaded CESIL program by executing each of its statements in turn on the VM.
    *
    * @param input     the sequence of integer values provided as input to the CESIL program. This
    *                  parameter has a default (no input values).
    * @param showSteps if set to `true` the state of the VM will be printed after every statement
    *                  execution. This parameter has a default (`false`).
    * @return the result of running the program - the output stream.
    */
  def run(input: Input = List(), showSteps: Boolean = false): Output = synchronized {

    def step(): Unit = {
      if (pc == end)
        throw new Exception("step: Program at end!")
      stepCounter = stepCounter + 1
      var nextStep = pc + 1
      program(pc) match {
        case LOAD(variable: Var)     => acc = variables.getOrElse(variable.varid, 0)
        case LOAD(value: Val)        => acc = value.literal
        case STORE(variable)         => variables += (variable.varid -> acc)
        case JUMP(label)             => nextStep = labels.getOrElse(label, end)
        case JINEG(label)            => if (acc < 0) nextStep = labels.getOrElse(label, end)
        case JIPOS(label)            => if (acc > 0) nextStep = labels.getOrElse(label, end)
        case JIZERO(label)           => if (acc == 0) nextStep = labels.getOrElse(label, end)
        case PRINT(string)           => outputStream = outputStream + string
        case LINE                    => outputStream = outputStream + "\n"
        case IN                      => if (input.isEmpty) acc = 0 else {
          acc = inputStream.head
          inputStream = inputStream.tail
        }
        case OUT                     => outputStream = outputStream + acc.toString
        case ADD(variable: Var)      => acc = acc + variables.getOrElse(variable.varid, 0)
        case ADD(value: Val)         => acc = acc + value.literal
        case SUBTRACT(variable: Var) => acc = acc - variables.getOrElse(variable.varid, 0)
        case SUBTRACT(value: Val)    => acc = acc - value.literal
        case MULTIPLY(variable: Var) => acc = acc * variables.getOrElse(variable.varid, 0)
        case MULTIPLY(value: Val)    => acc = acc * value.literal
        case DIVIDE(variable: Var)   => acc = acc / variables.getOrElse(variable.varid, 0)
        case DIVIDE(value: Val)      => acc = acc / value.literal
        case MODULO(variable: Var)   => acc = acc % variables.getOrElse(variable.varid, 0)
        case MODULO(value: Val)      => acc = acc % value.literal
        case PUSH                    => stack.push(acc)
        case POP                     => acc = stack.pop
        case SWAP                    => val temp = acc; acc = stack.pop; stack.push(temp)
        case NOP                     => //
        case HALT                    => nextStep = end
      }
      if (traceOn) {
        println(s"Just executed statement $pc (${program(pc)})")
        println(this.toString)
      }
      pc = nextStep
    }

    outputStream = ""
    traceOn = showSteps
    variables = new mutable.HashMap()
    stack = new mutable.Stack()
    stepCounter = 0
    inputStream = input
    pc = 0
    while (pc != end)
      step()
    outputStream
  }
}