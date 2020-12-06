/*
 * CESIL+ language definition and macros
 * Author: David Smallwood
 * January 2020
 */

package cesil

import scala.language.implicitConversions

/**
 * CESIL is an old computer programming language developed by
 * <a href="https://en.wikipedia.org/wiki/International_Computers_Limited">ICL</a> for teaching the fundamentals of
 * computer programming in schools. The acronym stands for
 * `C`omputer `E`ducation in `S`chools `I`nstruction `L`anguage. Various nostalgic posts about the
 * language can be found on the internet and there is a sparse
 * <a href="https://en.wikipedia.org/wiki/CESIL">Wikipedia entry</a>.
 *
 * CESIL is similar to assembly code and really is very limited. The language provided here extends CESIL's 14
 * instructions by adding a further six. These are `NOP` (no operation); `PUSH` (we added a stack); `POP` (to
 * remove values from the stack); `SWAP` (exchange the value in the accumulator with the top of the stack);
 * `JIPOS` (jump if positive); and `MODULO` to perform an arithmetic modulo operation (this partners nicely
 * with division). So this is really a superset of the CESIL language, but the latter provides the inspiration.
 */
object Lang {

  type NumLiteral = Int
  type VarIdent = String
  private val ReservedVarID: VarIdent = "#R"

  type Label = String
  private val AutoLabelPrefix = "#"

  type Statements = List[Statement]

  implicit def statementToStatements: Statement => Statements = List(_)

  type Input = List[NumLiteral]
  type Output = String

  /**
   *  A collective type for components of a postfix expression. These include
   *  variables (`Var`), values (`Val`), and the operators `Plus`, `Minus`, `Times`,
   *  `Div`, and `Mod`.
   */
  trait Token

  /**
   * Postfix expressions can be passed as parameters to some of the control
   * structure macros. E.g.
   * {{{
   * List(Var("x"), Var("y"), Var("z"), Times, Plus)
   * }}}
   * represents the expression `x + y*z`.
   */
  type PostfixExpr = List[Token]

  /**
   * This method automatically converts a `Token` to a `PostfixExpr`. For
   * example:  `Times` becomes `List(Times)` and `Var("x")` becomes `List(Var("x"))`.
   * Thus, instead of writing `List(Var("x"), Var("y"), Var("z"), Times, Plus)` one
   * can write `Var("x") :: Var("y") :: Var("z") :: Times :: Plus`. In this case the
   * final token `Plus` is lifted automatically to `List(Plus)` and the (right
   * associative) concatenation operator `::` joins each token to a list of tokens.
   * Whether or not to use `::` to connect tokens, rather than constructing a `List`
   * with parentheses, commas, and the word `List`, is just a matter of taste.
   */
  implicit def tokenToPostfixExpr: Token => PostfixExpr = List(_)

  /**
   * A superclass for `Var` and `Val`.
   */
  class VarVal

  /**
   * Represents variables. Thus, `Var("x")` is used to represent the variable `x`.
   * Variables are also tokens and can therefore appear in postfix expressions. A
   * small collection of overloaded assignment macros is provided. For example,
   * `Var("x") += Val(1)` has the effect of incrementing variable `x` by one. It
   * generates the relevant CESIL assembly code.
   */
  case class Var(varid: VarIdent) extends VarVal with Token {
    def :=(expr: PostfixExpr): Statements = eval(expr) ++ STORE(this)

    def :=(variable: Var): Statements = this := List(variable)

    def :=(value: Val): Statements = this := List(value)

    def +=(expr: PostfixExpr): Statements = eval(expr) ++
      ADD(this) ++
      STORE(this)

    def +=(variable: Var): Statements = this += List(variable)

    def +=(value: Val): Statements = this += List(value)

    def -=(expr: PostfixExpr): Statements = eval(expr) ++
      PUSH ++
      LOAD(this) ++
      SWAP ++
      STORE(this) ++
      POP ++
      SUBTRACT(this) ++
      STORE(this)

    def -=(variable: Var): Statements = this -= List(variable)

    def -=(value: Val): Statements = this -= List(value)

    def *=(expr: PostfixExpr): Statements = eval(expr) ++
      MULTIPLY(this) ++
      STORE(this)

    def *=(variable: Var): Statements = this *= List(variable)

    def *=(value: Val): Statements = this *= List(value)

    def /=(expr: PostfixExpr): Statements = eval(expr) ++
      PUSH ++
      LOAD(this) ++
      SWAP ++
      STORE(this) ++
      POP ++
      DIVIDE(this) ++
      STORE(this)

    def /=(variable: Var): Statements = this /= List(variable)

    def /=(value: Val): Statements = this /= List(value)

    def %=(expr: PostfixExpr): Statements = eval(expr) ++
      PUSH ++
      LOAD(this) ++
      SWAP ++
      STORE(this) ++
      POP ++
      MODULO(this) ++
      STORE(this)

    def %=(variable: Var): Statements = this %= List(variable)

    def %=(value: Val): Statements = this %= List(value)
  }

  /**
   * Represents values. Thus, `Val(42)` is used to represent the number 42.
   * Values are also tokens and can therefore appear in postfix expressions.
   */
  case class Val(literal: NumLiteral) extends VarVal with Token

  case object Plus extends Token

  case object Minus extends Token

  case object Times extends Token

  case object Div extends Token

  case object Mod extends Token

  /**
   * An abstract superclass representing all CESIL statements. Two subclasses are provided
   * for labelled and unlabelled statements respectively.
   */
  abstract class Statement {
    /** An abstract member function. All statements must respond to this predicate */
    def isLabelled: Boolean
    /** The inverse of `isLabelled`. It has a default implementation */
    def isUnlabelled: Boolean = !this.isLabelled
  }

  /**
   * Represents statements that do not have labels.
   */
  class Unlabelled extends Statement {
    override def isLabelled: Boolean = false
    /**
     * Provides a shorthand notation for labelling statements. Instead of writing
     * `Labelled("LABEL", PUSH)`, for example, it is possible to write
     * `"LABEL"::PUSH`. This syntactic sugar can be used to improve readability.
     */
    def ::(label: Label): Labelled = Labelled(label, this)
  }

  /**
   * Represents statements that do have labels.
   */
  case class Labelled(label: Label, unlabelled: Unlabelled) extends Statement {
    override def isLabelled: Boolean = true
  }

  /** Loads a variable or a value into the accumulator. */
  case class LOAD(varval: VarVal) extends Unlabelled

  /** Stores the accumulator in the given variable. */
  case class STORE(variable: Var) extends Unlabelled

  /** Sets the program counter to the statement labelled by `label`. */
  case class JUMP(label: Label) extends Unlabelled

  /** If the accumulator is negative, sets the program counter to the statement labelled by `label`. */
  case class JINEG(label: Label) extends Unlabelled

  /** If the accumulator is positive, sets the program counter to the statement labelled by `label`. */
  case class JIPOS(label: Label) extends Unlabelled

  /** If the accumulator is zero, sets the program counter to the statement labelled by `label`. */
  case class JIZERO(label: Label) extends Unlabelled

  /** Appends the `string` to the output stream. */
  case class PRINT(string: String) extends Unlabelled

  /** Appends a newline character to the output stream */
  case object LINE extends Unlabelled

  /** Reads a value (an Int) from the input stream into the accumulator */
  case object IN extends Unlabelled

  /** Outputs the value in the accumulator by appending its string representation to the output stream */
  case object OUT extends Unlabelled

  /** Adds to the accumulator a variable or value. */
  case class ADD(varval: VarVal) extends Unlabelled

  /** Subtracts from the accumulator a variable or value. */
  case class SUBTRACT(varval: VarVal) extends Unlabelled

  /** Multiplies the accumulator by a variable or value. */
  case class MULTIPLY(varval: VarVal) extends Unlabelled

  /** Divides the accumulator by a variable or value. */
  case class DIVIDE(varval: VarVal) extends Unlabelled

  /** Computes the modulus of the accumulator and a variable or value. */
  case class MODULO(varval: VarVal) extends Unlabelled

  /** Push the accumulator onto the stack. This does not change the accumulator. */
  case object PUSH extends Unlabelled

  /** Pops the stack and places the popped value into the accumulator. */
  case object POP extends Unlabelled

  /** Swaps the top of the stack with the accumulator. */
  case object SWAP extends Unlabelled

  /** No-operation. Does not change the state. */
  case object NOP extends Unlabelled

  /** Stops the program. */
  case object HALT extends Unlabelled

  /** Macros */

  /**
   * A macro that returns the CESIL code to
   * evaluate a postfix expression and place the result in the accumulator.
   * The macro makes use of a reserved variable name which should not be used
   * in any other context.
   */
  def eval(postFixExpr: PostfixExpr): Statements = {
    val reservedVar: Var = Var(ReservedVarID)
    postFixExpr.flatMap(_ match {
      case Val(value)    => LOAD(Val(value)) ++ PUSH
      case Var(variable) => LOAD(Var(variable)) ++ PUSH
      case Plus          => POP ++ STORE(reservedVar) ++ POP ++ ADD(reservedVar) ++ PUSH
      case Minus         => POP ++ STORE(reservedVar) ++ POP ++ SUBTRACT(reservedVar) ++ PUSH
      case Times         => POP ++ STORE(reservedVar) ++ POP ++ MULTIPLY(reservedVar) ++ PUSH
      case Div           => POP ++ STORE(reservedVar) ++ POP ++ DIVIDE(reservedVar) ++ PUSH
    }) ++
      POP
  }

  /**
   * A macro that returns the CESIL code to
   * increment the value of a variable by one.
   */
  def inc(variable: Var): Statements = variable += Val(1)

  /**
   * A macro that returns the CESIL code to
   * decrement the value of a variable by one.
   */
  def dec(variable: Var): Statements = variable -= Val(1)

  /*
   * WARNING:  The following are macros not control structures. Their effect is purely syntactic.
   * Be careful to avoid repeated shared references of code that includes labels. For example,
   * given
   *   val p1 = (L1::LOAD(x) ++ SUBTRACT(Val(1)) ++ JIZERO(L2) ++ OUT ++ JUMP(L1) ++ (L2::NOP)
   * then
   *   ifPositive{p1}
   * is safe, because p1 is only expanded once, whereas
   *   ifElsePositive{p1}{OUT ++ p1)
   * is not, because p1 is referenced twice. The duplication will cause each of the labels L1
   * and L2 to be duplicated.
   */

  private var labelCounter = 0

  private def newLabel(): Label = {
    labelCounter = labelCounter + 1
    AutoLabelPrefix + labelCounter
  }

  /* if ... else ... macros that use the value in the accumulator
   */

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator == 0)
   *   trueBlock
   * else
   *   falseBlock
   * }}}
   */
  def ifElseZero(trueBlock: Statements)(falseBlock: Statements): Statements = {
    val L1 = newLabel()
    val L2 = newLabel()
    JIZERO(L1) ++ falseBlock ++ JUMP(L2) ++ (L1 :: NOP) ++ trueBlock ++ (L2 :: NOP)
  }

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator != 0)
   *   trueBlock
   * else
   *   falseBlock
   * }}}
   */
  def ifElseNotZero(trueBlock: Statements)(falseBlock: Statements): Statements =
    ifElseZero(falseBlock)(trueBlock)

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator > 0)
   *   trueBlock
   * else
   *   falseBlock
   * }}}
   */
  def ifElsePositive(trueBlock: Statements)(falseBlock: Statements): Statements = {
    val L1 = newLabel()
    val L2 = newLabel()
    JIPOS(L1) ++ falseBlock ++ JUMP(L2) ++ (L1 :: NOP) ++ trueBlock ++ (L2 :: NOP)
  }

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator <= 0)
   *   trueBlock
   * else
   *   falseBlock
   * }}}
   */
  def ifElseNotPositive(trueBlock: Statements)(falseBlock: Statements): Statements =
    ifElsePositive(falseBlock)(trueBlock)

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator == 0)
   *   block
   * }}}
   */
  def ifZero(block: Statements): Statements =
    ifElseZero(block)(List())

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator != 0)
   *   block
   * }}}
   */
  def ifNotZero(block: Statements): Statements =
    ifElseNotZero(block)(List())

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator > 0)
   *   block
   * }}}
   */
  def ifPositive(block: Statements): Statements =
    ifElsePositive(block)(List())

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * if (accumulator <= 0)
   *   block
   * }}}
   */
  def ifNotPositive(block: Statements): Statements =
    ifElseNotPositive(block)(List())

  /* A multi-way switch that uses the value in the accumulator
   */

  /**
   * A macro that returns the CESIL code to perform a multi-way branch. For
   * example:
   * `switch(List( (valueSet1, block1), (valueSet2, block2), ... (valueSetN, blockN)))(defaultBlock)`
   * {{{
   * switch {
   *   if accumulator in valueSet1 => block1
   *   if accumulator in valueSet2 => block2
   *   :
   *   if accumulator in valueSetN => blockN
   *   otherwise => defaultBlock
   * }
   * }}}
   */
  def switch(cases: List[(Set[Val], Statements)])(defaultBlock: Statements): Statements = {
    val endLabel = newLabel()
    val labels = for (_ <- 1 to cases.length) yield newLabel()
    val (matches, blocks): (List[Set[Val]], List[Statements]) = cases.unzip
    val labelsMatches: List[(Label, Set[Val])] = labels.zip(matches).toList
    val labelsBlocks: List[(Label, Statements)] = labels.zip(blocks).toList
    PUSH ++
      labelsMatches.flatMap {
        case (label, matchSet) => matchSet.toList.flatMap {
          matchValue =>
            POP ++
              PUSH ++
              SUBTRACT(matchValue) ++
              JIZERO(label)
        }
      } ++
      POP ++ 
      PUSH ++
      defaultBlock ++
      JUMP(endLabel) ++
      labelsBlocks.flatMap {
        case (label, block) => Labelled(label, NOP) ++
          block ++
          JUMP(endLabel)
      } ++
      Labelled(endLabel, POP)
  }

  /* A collection of while loops that base the condition upon the value
   * in the accumulator.
   */

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * while (accumulator != 0)
   *   block
   * }}}
   */
  def whileNotZero(block: Statements): Statements = {
    val L1 = newLabel()
    val L2 = newLabel()
    (L1 :: JIZERO(L2)) ++ block ++ JUMP(L1) ++ (L2 :: NOP)
  }

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * while (accumulator > 0)
   *   block
   * }}}
   */
  def whilePositive(block: Statements): Statements = {
    val L1 = newLabel()
    val L2 = newLabel()
    val L3 = newLabel()
    (L1 :: JIPOS(L2)) ++ JUMP(L3) ++ (L2 :: NOP) ++ block ++ JUMP(L1) ++ (L3 :: NOP)
  }

  /**
   * A macro that returns the CESIL code to perform
   * {{{
   * while (accumulator <= 0)
   *   block
   * }}}
   */
  def whileNotPositive(block: Statements): Statements = {
    val L1 = newLabel()
    val L2 = newLabel()
    (L1 :: JIPOS(L2)) ++ block ++ JUMP(L1) ++ (L2 :: NOP)
  }

  /* The following all use overloaded signatures to prime the accumulator before calling
   * one of the primitive macros above. They are just syntactic sugar and can be safely
   * ignored on a first reading.
   */

  def ifElseLT(variable1: Var, variable2: Var)(trueBlock: Statements)(falseBlock: Statements): Statements =
    LOAD(variable2) ++ SUBTRACT(variable1) ++ ifElsePositive(trueBlock)(falseBlock)

  def ifElseLT(variable: Var, value: Val)(trueBlock: Statements)(falseBlock: Statements): Statements =
    LOAD(value) ++ SUBTRACT(variable) ++ ifElsePositive(trueBlock)(falseBlock)

  def ifElseLT(value: Val, variable: Var)(trueBlock: Statements)(falseBlock: Statements): Statements =
    LOAD(variable) ++ SUBTRACT(value) ++ ifElsePositive(trueBlock)(falseBlock)

  def ifElseEQ(variable1: Var, variable2: Var)(trueBlock: Statements)(falseBlock: Statements): Statements =
    LOAD(variable2) ++ SUBTRACT(variable1) ++ ifElseZero(trueBlock)(falseBlock)

  def ifElseEQ(variable: Var, value: Val)(trueBlock: Statements)(falseBlock: Statements): Statements =
    LOAD(value) ++ SUBTRACT(variable) ++ ifElseZero(trueBlock)(falseBlock)

  def ifElseEQ(value: Val, variable: Var)(trueBlock: Statements)(falseBlock: Statements): Statements =
    LOAD(variable) ++ SUBTRACT(value) ++ ifElseZero(trueBlock)(falseBlock)

  def ifElseLE(variable1: Var, variable2: Var)(trueBlock: Statements)(falseBlock: Statements): Statements =
    ifElseLT(variable1, variable2)(trueBlock)(ifElseEQ(variable1, variable2)(trueBlock)(falseBlock))

  def ifElseLE(variable: Var, value: Val)(trueBlock: Statements)(falseBlock: Statements): Statements =
    ifElseLT(variable, value)(trueBlock)(ifElseEQ(variable, value)(trueBlock)(falseBlock))

  def ifElseLE(value: Val, variable: Var)(trueBlock: Statements)(falseBlock: Statements): Statements =
    ifElseLT(value, variable)(trueBlock)(ifElseEQ(value, variable)(trueBlock)(falseBlock))

  def ifElseGT(variable1: Var, variable2: Var): Statements => Statements => Statements =
    ifElseLT(variable2, variable1)

  def ifElseGT(variable: Var, value: Val): Statements => Statements => Statements =
    ifElseLT(value, variable)

  def ifElseGT(value: Val, variable: Var): Statements => Statements => Statements =
    ifElseLT(variable, value)

  def ifElseGE(variable1: Var, variable2: Var): Statements => Statements => Statements =
    ifElseLE(variable2, variable1)

  def ifElseGE(variable: Var, value: Val): Statements => Statements => Statements =
    ifElseLE(value, variable)

  def ifElseGE(value: Val, variable: Var): Statements => Statements => Statements =
    ifElseLE(variable, value)

  def ifElseNE(variable1: Var, variable2: Var): Statements => Statements => Statements =
    ifElseEQ(variable2, variable1)

  def ifElseNE(variable: Var, value: Val): Statements => Statements => Statements =
    ifElseEQ(value, variable)

  def ifElseNE(value: Val, variable: Var): Statements => Statements => Statements =
    ifElseEQ(variable, value)

  def ifLT(variable1: Var, variable2: Var): Statements => Statements =
    ifElseLT(variable1, variable2)(_)(List())

  def ifLT(variable: Var, value: Val): Statements => Statements =
    ifElseLT(variable, value)(_)(List())

  def ifLT(value: Val, variable: Var): Statements => Statements =
    ifElseLT(value, variable)(_)(List())

  def ifGT(variable1: Var, variable2: Var): Statements => Statements =
    ifElseGT(variable1, variable2)(_)(List())

  def ifGT(variable: Var, value: Val): Statements => Statements =
    ifElseGT(variable, value)(_)(List())

  def ifGT(value: Val, variable: Var): Statements => Statements =
    ifElseGT(value, variable)(_)(List())

  def ifLE(variable1: Var, variable2: Var): Statements => Statements =
    ifElseLE(variable1, variable2)(_)(List())

  def ifLE(variable: Var, value: Val): Statements => Statements =
    ifElseLE(variable, value)(_)(List())

  def ifLE(value: Val, variable: Var): Statements => Statements =
    ifElseLE(value, variable)(_)(List())

  def ifGE(variable1: Var, variable2: Var): Statements => Statements =
    ifElseGE(variable1, variable2)(_)(List())

  def ifGE(variable: Var, value: Val): Statements => Statements =
    ifElseGE(variable, value)(_)(List())

  def ifGE(value: Val, variable: Var): Statements => Statements =
    ifElseGE(value, variable)(_)(List())

  def ifEQ(variable1: Var, variable2: Var): Statements => Statements =
    ifElseEQ(variable1, variable2)(_)(List())

  def ifEQ(variable: Var, value: Val): Statements => Statements =
    ifElseEQ(variable, value)(_)(List())

  def ifEQ(value: Val, variable: Var): Statements => Statements =
    ifElseEQ(value, variable)(_)(List())

  def ifNE(variable1: Var, variable2: Var): Statements => Statements =
    ifElseNE(variable1, variable2)(_)(List())

  def ifNE(variable: Var, value: Val): Statements => Statements =
    ifElseNE(variable, value)(_)(List())

  def ifNE(value: Val, variable: Var): Statements => Statements =
    ifElseNE(value, variable)(_)(List())

  def whileNotZero(expr: PostfixExpr)(block: Statements): Statements = {
    val test = eval(expr)
    test ++ whileNotZero(block ++ test)
  }

  def whileNotZero(variable: Var): Statements => Statements =
    whileNotZero(List(variable))

  def whilePositive(expr: PostfixExpr)(block: Statements): Statements = {
    val test = eval(expr)
    test ++ whilePositive(block ++ test)
  }

  def whilePositive(variable: Var): Statements => Statements =
    whilePositive(List(variable))

  def whileNotPositive(expr: PostfixExpr)(block: Statements): Statements = {
    val test = eval(expr)
    test ++ whileNotPositive(block ++ test)
  }

  def whileNotPositive(variable: Var): Statements => Statements =
    whileNotPositive(List(variable))

  def whileNE(variable1: Var, variable2: Var)(block: Statements): Statements = {
    val test = List(LOAD(variable2), SUBTRACT(variable1))
    test ++ whileNotZero(block ++ test)
  }

  def whileNE(variable: Var, value: Val)(block: Statements): Statements = {
    val test = List(LOAD(value), SUBTRACT(variable))
    test ++ whileNotZero(block ++ test)
  }

  def whileNE(value: Val, variable: Var)(block: Statements): Statements = {
    val test = List(LOAD(variable), SUBTRACT(value))
    test ++ whileNotZero(block ++ test)
  }

  def whileLT(variable1: Var, variable2: Var)(block: Statements): Statements = {
    val test = List(LOAD(variable2), SUBTRACT(variable1))
    test ++ whilePositive(block ++ test)
  }

  def whileLT(variable: Var, value: Val)(block: Statements): Statements = {
    val test = List(LOAD(value), SUBTRACT(variable))
    test ++ whilePositive(block ++ test)
  }

  def whileLT(value: Val, variable: Var)(block: Statements): Statements = {
    val test = List(LOAD(variable), SUBTRACT(value))
    test ++ whilePositive(block ++ test)
  }

  def whileLE(variable1: Var, variable2: Var)(block: Statements): Statements = {
    val test = List(LOAD(variable1), SUBTRACT(variable2))
    test ++ whileNotPositive(block ++ test)
  }

  def whileLE(variable: Var, value: Val)(block: Statements): Statements = {
    val test = List(LOAD(variable), SUBTRACT(value))
    test ++ whileNotPositive(block ++ test)
  }

  def whileLE(value: Val, variable: Var)(block: Statements): Statements = {
    val test = List(LOAD(value), SUBTRACT(variable))
    test ++ whileNotPositive(block ++ test)
  }
}