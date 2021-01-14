/*
 * CESIL+ analyser testing
 * Author: David Smallwood
 * January 2020
 */

package cesil
import org.junit.Test
import junit.framework.TestCase
import org.junit.Assert._
import Lang._
import Analyser._

class AnalyserTest {
  
  val emptyProgram: Statements = List()
  val simple0: Statements = List(NOP)
  val simple1: Statements = List(Labelled("one",NOP))
  val simple2: Statements = List(NOP, NOP)
  val simple3: Statements = List(NOP, Labelled("one",NOP))
  val simple4: Statements = List(Labelled("one",NOP), NOP)
  val simple5: Statements = List(Labelled("one",NOP), Labelled("two",NOP))
  val simple6: Statements = List(PUSH, POP)
  val simple7: Statements = List(PUSH, NOP, POP)
  val simple8: Statements = List(PUSH, POP, PUSH, POP)
  val simple9: Statements = List(PUSH, POP, Labelled("seven",PUSH), POP)
  val simpleA: Statements = List(PUSH, PUSH, POP, POP)
  val simpleB: Statements = List(PUSH, PUSH, PUSH, POP, POP, POP)
  val simpleC: Statements = List(PUSH, PUSH, PUSH, POP, POP, NOP, POP)
  val simpleD: Statements = List(PUSH, POP, Labelled("one",NOP), OUT)
  val simpleH: Statements = List(HALT)
  val onlyThreePush: Statements = List(PUSH, PUSH, PUSH)
  val onlyThreePop: Statements = List(POP, POP, POP)
  
  val oneRef: Statements = List(JUMP("one"))
  val twoRefs: Statements = List(JINEG("two"), JUMP("one"))
  val threeRefs: Statements = twoRefs ++ List(IN, JIZERO("one"), JINEG("three"), JIPOS("two")) ++ oneRef
  val labelledOneRef: Statements = List(Labelled("one", JUMP("two")))
  val labelledTwoRefs: Statements = List(Labelled("one", JUMP("two")), Labelled("two", JUMP("one")))
  val mixedRefs: Statements = List(Labelled("one", JUMP("two")), JUMP("one"), IN, JINEG("three"))
  
  val oneMissingLabel: Statements = List(Labelled("one", JINEG("two")), JUMP("one"), IN, Labelled("two", JINEG("three")))
  val twoMissingLabels: Statements = List(Labelled("three", IN), JINEG("one"), JIPOS("two"), JUMP("three"))
  val oneRedundantLabel: Statements = List(Labelled("start", IN), Labelled("one", JUMP("two")), Labelled("two", JUMP("one")))
  val threeRedundantLabels: Statements = List(Labelled("start", IN), Labelled("one", PUSH), Labelled("two", IN),
      PUSH, Labelled("three", IN), Labelled("four", JINEG("start")), POP, JUMP("three"), OUT, HALT)
  val missingLabelRefs: Statements = List()

  /*
   * Testing listStatementTypesUsed (10 tests)
   */
  
  @Test def testListStatementTypesUsedWithEmptyProgram {
    assertTrue(listStatementTypesUsed(emptyProgram).isEmpty)
  }
    
  @Test def testListStatementTypesUsedWithHaltProgram {
    assertEquals(List("HALT"), listStatementTypesUsed(simpleH))
  }
  
  @Test def testListStatementTypesUsedWithNOP {
    assertEquals(List("NOP"), listStatementTypesUsed(simple0))
  }
  
  @Test def testListStatementTypesUsedWithLabelledNOP {
    assertEquals(List("NOP"), listStatementTypesUsed(simple1))
  }
  
  @Test def testListStatementTypesUsedWithTwoNOPs {
    assertEquals(List("NOP"), listStatementTypesUsed(simple2))
  }  
  
  @Test def testListStatementTypesUsedWithUnlabelledAndLabelledNOP {
    assertEquals(List("NOP"), listStatementTypesUsed(simple3))
  }
  
  @Test def testListStatementTypesUsedWithLabelledAndUnlabelledNOP {
    assertEquals(List("NOP"), listStatementTypesUsed(simple4))
  }
  
  @Test def testListStatementTypesUsedWithPUSHPOPNOP {
    assertEquals(List("NOP", "POP", "PUSH"), listStatementTypesUsed(simpleC))
  }
  
  @Test def testListStatementTypesUsedWithExamplesSignProgram {
    val expected = List("HALT", "IN", "JINEG", "JIPOS", "JIZERO", "JUMP", "LOAD",
        "NOP", "OUT", "POP", "PRINT", "PUSH")
    assertEquals(expected, listStatementTypesUsed(Examples.sign))
  }
  
  @Test def testListStatementTypesUsedWithExamplesAsBinaryProgram {
    val expected = List("ADD", "DIVIDE", "HALT", "IN", "JIPOS", "JIZERO", "JUMP",
        "LINE", "LOAD", "MODULO", "NOP", "OUT", "POP", "PUSH", "STORE", "SUBTRACT", "SWAP")
    assertEquals(expected, listStatementTypesUsed(Examples.asBinary))
  }
  
  /*
   * Testing getLabelRefs (10 tests)
   */
  
  @Test def getLabelRefsWithEmptyProgram {
    assertTrue(getLabelRefs(emptyProgram).isEmpty)
  }
    
  @Test def getLabelRefsWithHaltProgram {
    assertTrue(getLabelRefs(simpleH).isEmpty)
  }
  
  @Test def getLabelRefsWithOneLabelProgramNoRefs {
    assertTrue(getLabelRefs(simple1).isEmpty)
  } 
  
  @Test def getLabelRefsWithNonEmptyProgramNoRefs {
    assertTrue(getLabelRefs(simpleC).isEmpty)
  }
    
  @Test def getLabelRefsWithOneStatementAndOneRef {
    assertEquals(List("one"), getLabelRefs(oneRef))
  }
  
  @Test def getLabelRefsWithTwoStatementAndTwoRefs {
    assertEquals(List("one", "two"), getLabelRefs(twoRefs))
  }
  
  @Test def getLabelRefsWithProgramWithThreeRefs {
    assertEquals(List("one", "three", "two"), getLabelRefs(threeRefs))
  }
  
  @Test def getLabelRefsWithProgramWithTwoLabelledRefs {
    assertEquals(List("one", "two"), getLabelRefs(labelledTwoRefs))
  }
  
  @Test def getLabelRefsWithProgramWithMixedRefs {
    assertEquals(List("one", "three", "two"), getLabelRefs(mixedRefs))
  }
     
  @Test def getLabelRefsWithProgramWithTwoMissingLabels {
    assertEquals(List("one", "three", "two"), getLabelRefs(twoMissingLabels))
  }
    
  /*
   * Testing redundantLabels (5 tests)
   */

  @Test def redundantLabelsWithEmptyProgram {
    assertTrue(redundantLabels(emptyProgram).isEmpty)
  }
  
  @Test def redundantLabelsWithNoLabelsOrRefs {
    assertTrue(redundantLabels(simpleC).isEmpty)
  }
  
  @Test def redundantLabelsWithNoRedundancy {
    assertTrue(redundantLabels(labelledTwoRefs).isEmpty)
  }

  @Test def redundantLabelsWithOneRedundantLabel {
    assertEquals(List("start"), redundantLabels(oneRedundantLabel))
  }
    
  @Test def redundantLabelsWithThreeRedundantLabels {
    assertEquals(List("four", "one", "two"), redundantLabels(threeRedundantLabels))
  }
  
  /*
   * Testing missingLabels (5 tests)
   */

  @Test def missingLabelsWithEmptyProgram {
    assertTrue(missingLabels(emptyProgram).isEmpty)
  }
      
    @Test def missingLabelsWithNoLabelsOrRefs {
    assertTrue(missingLabels(simpleC).isEmpty)
  }

  @Test def missingLabelsWithNoMissingLabels {
    assertTrue(missingLabels(labelledTwoRefs).isEmpty)
  }
      
  @Test def missingLabelsWithTwoMissingLabels {
    assertEquals(List("one", "two"), missingLabels(twoMissingLabels))
  }
        
  @Test def missingLabelsWithOneMissingLabel {
    assertEquals(List("three"), missingLabels(oneMissingLabel))
  }
      

  /*
   * Testing stripPushPopPairs (10 tests)
   */
  
  @Test def testStripPushPopPairsWithEmpty {
    assertEquals(emptyProgram, stripPushPopPairs(emptyProgram))
  }
  
  @Test def testStripPushPopPairsWithNoPushPops1 {
    assertEquals(simple1, stripPushPopPairs(simple1))
  }
  
  @Test def testStripPushPopPairsWithNoPushPops2 {
    assertEquals(simple2, stripPushPopPairs(simple2))
  }
  
  @Test def testStripPushPopPairsWithOnlyThreePush {
    assertEquals(onlyThreePush, stripPushPopPairs(onlyThreePush))
  }
  
  @Test def testStripPushPopPairsWithOnlyThreePop {
    assertEquals(onlyThreePop, stripPushPopPairs(onlyThreePop))
  }
  
  @Test def testStripPushPopPairsWithOnePair {
    assertEquals(emptyProgram, stripPushPopPairs(simple6))
  }
  
  @Test def testStripPushPopPairsWithTwoPairs {
    assertEquals(emptyProgram, stripPushPopPairs(simple8))
  }
  
  @Test def testStripPushPopPairsWithNestedPair {
    assertEquals(List(PUSH, POP), stripPushPopPairs(simpleA))
  }
  
  @Test def testStripPushPopPairsWithDoubleNestedPair {
    assertEquals(List(PUSH, PUSH, POP, POP), stripPushPopPairs(simpleB))
  }
  
  @Test def testStripPushPopPairsWithTwoPairsOneLabelled {
    assertEquals(List(Labelled("seven", NOP)), stripPushPopPairs(simple9))
  }

  
  /*
   * Testing stripUnlabelledNops (5 tests)
   */
      
  @Test def testStripUnlabelledNopsWithNoNops{
    assertEquals(List(PUSH, PUSH, PUSH, POP, POP, POP), stripUnlabelledNOPs(simpleB))
  }
        
  @Test def testStripUnlabelledNopsWithOneInMiddle{
    assertEquals(List(PUSH, POP), stripUnlabelledNOPs(simple7))
  }
  
  @Test def testStripUnlabelledNopsWithOneUnlabelledNop{
    assertTrue(stripUnlabelledNOPs(simple0).isEmpty)
  }
    
  @Test def testStripUnlabelledNopsWithEmptyProgram{
    assertTrue(stripUnlabelledNOPs(emptyProgram).isEmpty)
  }
  
  @Test def testStripUnlabelledNopsWithOneLabelledOneNot{
    assertEquals(List(Labelled("one", NOP)), stripUnlabelledNOPs(simple4))
  }
  
  
  /*
   * Testing stripLabelledNops (5 tests)
   */
  
  @Test def testStripLabelledNopsWithEmptyProgram{
    assertTrue(stripLabelledNOPs(emptyProgram).isEmpty)
  }
  
  @Test def testStripLabelledNopsWithOneLabelledNop{
    assertEquals(simple1, stripLabelledNOPs(simple1))
  }
  
  @Test def testStripLabelledNopsWithTwoLabelledNops{
    assertEquals(List(Labelled("two",NOP)), stripLabelledNOPs(simple5))
  }
  
  @Test def testStripLabelledNopsWithOneUnlabelledOneNot{
    assertEquals(List(PUSH, POP, Labelled("one",OUT)), stripLabelledNOPs(simpleD))
  }
  
  @Test def testStripLabelledNopsWithOneUnlabelledNopEmbedded{
    assertEquals(simpleC, stripLabelledNOPs(simpleC))
  }
  
  
  /*
   * Testing stripNops and stripPushPopPairs (9 tests)
   */
  
  @Test def testStripNopsAndPushPops1{
    assertEquals(simpleA, stripPushPopPairs(stripNOPs(simpleC)))
  }
  
  @Test def testStripNopsAndPushPops2{
    val prog = simple7 ++ simple4 ++ simple6 ++ simple0 ++ simpleA ++ simple9
    //List(PUSH, NOP, POP, Labelled("one",NOP), NOP, PUSH, POP, NOP, PUSH, PUSH, POP, POP, PUSH, POP, Labelled("seven",PUSH), POP)
    //stripNops => List(PUSH, POP, Labelled("one", PUSH), POP, PUSH, PUSH, POP, POP, PUSH, POP, Labelled("seven",PUSH), POP)
    //stripPushPopPairs => List(PUSH, POP, Labelled("one",NOP))
    assertEquals(List(PUSH, POP, Labelled("one", PUSH), POP, PUSH, PUSH, POP, POP, PUSH, POP, Labelled("seven",PUSH), POP), stripNOPs(prog))
    assertEquals(List(Labelled("one",NOP), PUSH, POP, Labelled("seven",NOP)), stripPushPopPairs(stripNOPs(prog)))
  }
  
  @Test def testStripNopsAndPushPops3{
    val prog = simple7 ++ simple4 ++ simple6 ++ simple0 ++ simpleA ++ simple9
    //List(PUSH, NOP, POP, Labelled("one",NOP), NOP, PUSH, POP, NOP, PUSH, PUSH, POP, POP, PUSH, POP, Labelled("seven",PUSH), POP)
    //stripPushPopPairs => List(PUSH, NOP, POP, Labelled("one",NOP), NOP, NOP, PUSH, POP, Labelled("seven",NOP))
    //strippedNops => List(PUSH, POP, Labelled("one",PUSH), POP, Labelled("seven",NOP))
    //stripPushPopPairs => List(Labelled("one",NOP), Labelled("seven",NOP))
    assertEquals(List(PUSH, NOP, POP, Labelled("one",NOP), NOP, NOP, PUSH, POP, Labelled("seven",NOP)), stripPushPopPairs(prog))
    assertEquals(List(PUSH, POP, Labelled("one",PUSH), POP, Labelled("seven",NOP)), stripNOPs(stripPushPopPairs(prog)))
    assertEquals(List(Labelled("one",NOP), Labelled("seven",NOP)), stripPushPopPairs(stripNOPs(stripPushPopPairs(prog))))
  }
  
  
  @Test def testStripNopsAndPushPops4 {
    val vm1 = new VirtualMachine()
    val vm2 = new VirtualMachine()
    vm1.load(Examples.asBinary)
    vm2.load(stripNOPs(stripPushPopPairs(Examples.asBinary)))
    assertEquals(vm1.run(List(42)), vm2.run(List(42)))
  }

  @Test def testStripNopsAndPushPops5 {
    val vm1 = new VirtualMachine()
    val vm2 = new VirtualMachine()
    vm1.load(Examples.daysInMonth)
    vm2.load(stripNOPs(stripPushPopPairs(Examples.daysInMonth)))
    assertEquals(vm1.run(List(2, 4, 0)), vm2.run(List(2, 4, 0)))
  }
  
  @Test def testStripNopsAndPushPops6 {
    val vm1 = new VirtualMachine()
    val vm2 = new VirtualMachine()
    vm1.load(Examples.pythagoreanTriples)
    vm2.load(stripNOPs(stripPushPopPairs(Examples.pythagoreanTriples)))
    assertEquals(vm1.run(List(100)), vm2.run(List(100)))
  }

  @Test def testStripNopsAndPushPops7 {
    val vm1 = new VirtualMachine()
    val vm2 = new VirtualMachine()
    vm1.load(Examples.hcf)
    vm2.load(stripNOPs(stripPushPopPairs(Examples.hcf)))
    assertEquals(vm1.run(List(36, 82)), vm2.run(List(36, 82)))
  }
  
  @Test def testStripNopsAndPushPops8 {
    val vm1 = new VirtualMachine()
    val vm2 = new VirtualMachine()
    vm1.load(Examples.smallestOfThree)
    vm2.load(stripNOPs(stripPushPopPairs(Examples.smallestOfThree)))
    assertEquals(vm1.run(List(42, 17, 59)), vm2.run(List(42, 17, 59)))
  }

  @Test def testStripNopsAndPushPops9 {
    val vm1 = new VirtualMachine()
    val vm2 = new VirtualMachine()
    vm1.load(Examples.sumOfN)
    vm2.load(stripNOPs(stripPushPopPairs(Examples.sumOfN)))
    assertEquals(vm1.run(List(5, 13, 17, 19, 23, 27)), vm2.run(List(5, 13, 17, 19, 23, 27)))
  }

  @Test def testStripNopsAndPushPops10 {
    val vm1 = new VirtualMachine()
    val vm2 = new VirtualMachine()
    vm1.load(Examples.sign)
    vm2.load(stripNOPs(stripPushPopPairs(Examples.sign)))
  }

}