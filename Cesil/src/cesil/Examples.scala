/*
 * CESIL+ example programs
 * Author: David Smallwood
 * January 2020
 */

package cesil

import cesil.Lang._
import cesil.Analyser._
import cesil.Pretty._

/**
 * A runnable collection of example CESIL+ programs.
 */
object Examples {

  /**
   * Prints out the sign of each number input until zero is read.
   * The example contains some redundant lines.
   */
  val sign = Labelled("AGAIN", IN) ++
             JINEG("NEG") ++
             JIPOS("POS") ++
             LOAD(Val(0)) ++
             JUMP("PRN") ++
             Labelled("NEG", LOAD(Val(-1))) ++
             JUMP("PRN") ++
             Labelled("POS", LOAD(Val(1))) ++
             Labelled("PRN", OUT) ++
             Labelled("WHY1", NOP) ++
             Labelled("WHY2", PUSH) ++
             POP ++
             Labelled("WHY3", NOP) ++
             ifNotZero {
               PRINT(", ") ++
               JUMP("AGAIN")
             } ++
             HALT
  /**
   * Calculates the sum of N numbers. N is the first value input. Then
   * a loop for N times reads in new values and adds them to the running
   * total.
   */
  val sumOfN = IN ++
               STORE(Var("N")) ++
               LOAD(Val(0)) ++
               STORE(Var("SUM")) ++
               LOAD(Val(0)) ++
               STORE(Var("I")) ++
               whileLT(Var("I"), Var("N")) {
                 IN ++
                 ADD(Var("SUM")) ++
                 STORE(Var("SUM")) ++
                 inc(Var("I"))
               } ++
               LOAD(Var("SUM")) ++
               PRINT("Sum = ") ++
               OUT ++
               HALT

  /**
   * Converts a non-negative input value to 8-bit binary and prints this out.
   */
  val asBinary = IN ++
                 STORE(Var("D")) ++
                 LOAD(Val(8)) ++
                 STORE(Var("N")) ++
                 whileNotZero(Var("N")) {
                   LOAD(Var("D")) ++
                   MODULO(Val(2)) ++
                   PUSH ++
                   LOAD(Var("D")) ++
                   DIVIDE(Val(2)) ++
                   STORE(Var("D")) ++
                   dec(Var("N"))
                 } ++
                 whileLE(Var("N"), Val(7)) {
                   POP ++
                   OUT ++
                   inc(Var("N"))
                 } ++
                 LINE ++
                 HALT
  /**
   * Reads in two numbers and prints out their highest common factor.
   */
  val hcf = IN ++
            STORE(Var("x")) ++
            IN ++
            STORE(Var("y")) ++
            whileNE(Var("x"), Var("y")) {
              whileLT(Var("y"), Var("x")) {
                Var("x") -= Var("y")
              } ++
              whileLT(Var("x"), Var("y")) {
                Var("y") -= Var("x")
              }
            } ++
            LOAD(Var("x")) ++
            OUT ++
            HALT

  /**
   * Reads in a number n, and then computes all the Pythogerean triples (x,y,z)
   * such that 1 <= x <= y <= z <= n. Each answer is printed out.
   */
  val pythagoreanTriples =
    IN ++
    STORE(Var("n")) ++
    (Var("x") := Val(1)) ++
    whileLT(Var("x"), Var("n")) {
      (Var("y") := Var("x")) ++
      whileLT(Var("y"), Var("n")) {
        (Var("z") := Var("y")) ++
        whileLT(Var("z"), Var("n")) {
          eval(
            Var("x") :: Var("x") :: Times :: Var("y") :: Var("y") :: Times :: Plus ::
            Var("z") :: Var("z") :: Times :: Minus
          ) ++
          ifZero {
            PRINT("<") ++
            LOAD(Var("x")) ++
            OUT ++
            PRINT(", ") ++
            LOAD(Var("y")) ++
            OUT ++
            PRINT(", ") ++
            LOAD(Var("z")) ++
            OUT ++
            PRINT(">") ++
            LINE
          } ++
          inc(Var("z"))
        } ++
        inc(Var("y"))
      } ++
      inc(Var("x"))
    } ++
    HALT

  /**
   * Inputs three numbers and prints out the least.
   */
  val smallestOfThree =
    IN ++
    STORE(Var("a")) ++
    IN ++
    STORE(Var("b")) ++
    IN ++
    STORE(Var("c")) ++
    ifElseLT(Var("a"), Var("b")) {
      ifElseLT(Var("a"), Var("c")) {
        LOAD(Var("a"))
      } {
        LOAD(Var("c"))
      }
    } {
      ifElseLT(Var("b"), Var("c")) {
        LOAD(Var("b"))
      } {
        LOAD(Var("c"))
      }
    } ++
    OUT ++
    HALT

  /**
   * Reads a month (1 = January, 2 = February, ..., 12 = December) and prints out the
   * number of days in the month. If the input value is not in the correct range then
   * the output states that the number of days is undefined. The program repeats until
   * a zero is read on the input stream.
   */
  val daysInMonth = IN ++
                    whileNotZero {
                      PRINT("Month ") ++
                      OUT ++
                      PRINT(" has ") ++
                      switch(
                        List(
                          (Set(Val(1), Val(3), Val(5), Val(7), Val(8), Val(10), Val(12)),
                            PRINT("31")
                          ),
                          (Set(Val(4), Val(6), Val(9), Val(11)),
                            PRINT("30")
                          )
                        )
                      ) {
                        SUBTRACT(Val(2)) ++
                        ifElseZero {
                          PRINT("28 or 29 (if leap year)")
                        } {
                          PRINT("an undefined number of")
                        }
                      } ++
                      PRINT(" days.") ++
                      LINE ++
                      IN
                    } ++
                    HALT


  def main(args: Array[String]): Unit = {
    val vm: VirtualMachine = new VirtualMachine

    def runExample(name: String, program: Statements, input: List[Int]) {
      println(s"Running program $name with input=$input")
      println(verticalFormat(program))
      vm.load(program)
      val output = vm.run(input)
      println(output)
    }

    runExample("sign", sign, List(-1, 1, -127, 255, 0))
    runExample("sumOfN", sumOfN, List(10, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    runExample("asBinary", asBinary, List(32))
    runExample("hcf", hcf, List(36, 82))
    runExample("pythagoreanTriples", pythagoreanTriples, List(20))
    runExample("smallestOfThree", smallestOfThree, List(13, -4, 0))
    runExample("pythagoreanTriples", pythagoreanTriples, List(20))
    runExample("daysInMonth", daysInMonth, List(4, 13, 2, 12, 0))
    
  }
}
