package de.fosd.vainterpretation.interpreter.test

import de.fosd.vainterpretation.interpreter.test.TestConstraints._
import de.fosd.typechef.featureexpr.FeatureExpr
import org.junit._
import Assert._
import de.fosd.vainterpretation.parser.WhileParser
import de.fosd.vainterpretation.interpreter.VariableProgram
import de.fosd.vainterpretation.interpreter.ProgramUtils
import de.fosd.typechef.featureexpr.FeatureExprFactory


class GeneratedInterpreterTest {
  
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  val parser: WhileParser = new WhileParser()
    
  @Test
  def testCase00 = {
    val program = parser.parseFile("programs\\generated\\test00.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }
  
  @Test
  def testCase01 = {
    val program = parser.parseFile("programs\\generated\\test01.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase02 = {
    val program = parser.parseFile("programs\\generated\\test02.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase03 = {
    val program = parser.parseFile("programs\\generated\\test03.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase04 = {
    val program = parser.parseFile("programs\\generated\\test04.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase05 = {
    val program = parser.parseFile("programs\\generated\\test05.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase06 = {
    val program = parser.parseFile("programs\\generated\\test06.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase07 = {
    val program = parser.parseFile("programs\\generated\\test07.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase08 = {
    val program = parser.parseFile("programs\\generated\\test08.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase09 = {
    val program = parser.parseFile("programs\\generated\\test09.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase10 = {
    val program = parser.parseFile("programs\\generated\\test10.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase11 = {
    val program = parser.parseFile("programs\\generated\\test11.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase12 = {
    val program = parser.parseFile("programs\\generated\\test12.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase13 = {
    val program = parser.parseFile("programs\\generated\\test13.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase14 = {
    val program = parser.parseFile("programs\\generated\\test14.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase15 = {
    val program = parser.parseFile("programs\\generated\\test15.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase16 = {
    val program = parser.parseFile("programs\\generated\\test16.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase17 = {
    val program = parser.parseFile("programs\\generated\\test17.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase18 = {
    val program = parser.parseFile("programs\\generated\\test18.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase19 = {
    val program = parser.parseFile("programs\\generated\\test19.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase20 = {
    val program = parser.parseFile("programs\\generated\\test20.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase21 = {
    val program = parser.parseFile("programs\\generated\\test21.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase22 = {
    val program = parser.parseFile("programs\\generated\\test22.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase23 = {
    val program = parser.parseFile("programs\\generated\\test23.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase24 = {
    val program = parser.parseFile("programs\\generated\\test24.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase25 = {
    val program = parser.parseFile("programs\\generated\\test25.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase26 = {
    val program = parser.parseFile("programs\\generated\\test26.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase27 = {
    val program = parser.parseFile("programs\\generated\\test27.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase28 = {
    val program = parser.parseFile("programs\\generated\\test28.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase29 = {
    val program = parser.parseFile("programs\\generated\\test29.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase30 = {
    val program = parser.parseFile("programs\\generated\\test30.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase31 = {
    val program = parser.parseFile("programs\\generated\\test31.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase32 = {
    val program = parser.parseFile("programs\\generated\\test32.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase33 = {
    val program = parser.parseFile("programs\\generated\\test33.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase34 = {
    val program = parser.parseFile("programs\\generated\\test34.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase35 = {
    val program = parser.parseFile("programs\\generated\\test35.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase36 = {
    val program = parser.parseFile("programs\\generated\\test36.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase37 = {
    val program = parser.parseFile("programs\\generated\\test37.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase38 = {
    val program = parser.parseFile("programs\\generated\\test38.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase39 = {
    val program = parser.parseFile("programs\\generated\\test39.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase40 = {
    val program = parser.parseFile("programs\\generated\\test40.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase41 = {
    val program = parser.parseFile("programs\\generated\\test41.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase42 = {
    val program = parser.parseFile("programs\\generated\\test42.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase43 = {
    val program = parser.parseFile("programs\\generated\\test43.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase44 = {
    val program = parser.parseFile("programs\\generated\\test44.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase45 = {
    val program = parser.parseFile("programs\\generated\\test45.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase46 = {
    val program = parser.parseFile("programs\\generated\\test46.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase47 = {
    val program = parser.parseFile("programs\\generated\\test47.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase48 = {
    val program = parser.parseFile("programs\\generated\\test48.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase49 = {
    val program = parser.parseFile("programs\\generated\\test49.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase50 = {
    val program = parser.parseFile("programs\\generated\\test50.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase51 = {
    val program = parser.parseFile("programs\\generated\\test51.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase52 = {
    val program = parser.parseFile("programs\\generated\\test52.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase53 = {
    val program = parser.parseFile("programs\\generated\\test53.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase54 = {
    val program = parser.parseFile("programs\\generated\\test54.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase55 = {
    val program = parser.parseFile("programs\\generated\\test55.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase56 = {
    val program = parser.parseFile("programs\\generated\\test56.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase57 = {
    val program = parser.parseFile("programs\\generated\\test57.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase58 = {
    val program = parser.parseFile("programs\\generated\\test58.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase59 = {
    val program = parser.parseFile("programs\\generated\\test59.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase60 = {
    val program = parser.parseFile("programs\\generated\\test60.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase61 = {
    val program = parser.parseFile("programs\\generated\\test61.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase62 = {
    val program = parser.parseFile("programs\\generated\\test62.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase63 = {
    val program = parser.parseFile("programs\\generated\\test63.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase64 = {
    val program = parser.parseFile("programs\\generated\\test64.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase65 = {
    val program = parser.parseFile("programs\\generated\\test65.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase66 = {
    val program = parser.parseFile("programs\\generated\\test66.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase67 = {
    val program = parser.parseFile("programs\\generated\\test67.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase68 = {
    val program = parser.parseFile("programs\\generated\\test68.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase69 = {
    val program = parser.parseFile("programs\\generated\\test69.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase70 = {
    val program = parser.parseFile("programs\\generated\\test70.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase71 = {
    val program = parser.parseFile("programs\\generated\\test71.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase72 = {
    val program = parser.parseFile("programs\\generated\\test72.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase73 = {
    val program = parser.parseFile("programs\\generated\\test73.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase74 = {
    val program = parser.parseFile("programs\\generated\\test74.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase75 = {
    val program = parser.parseFile("programs\\generated\\test75.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase76 = {
    val program = parser.parseFile("programs\\generated\\test76.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase77 = {
    val program = parser.parseFile("programs\\generated\\test77.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase78 = {
    val program = parser.parseFile("programs\\generated\\test78.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase79 = {
    val program = parser.parseFile("programs\\generated\\test79.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase80 = {
    val program = parser.parseFile("programs\\generated\\test80.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase81 = {
    val program = parser.parseFile("programs\\generated\\test81.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase82 = {
    val program = parser.parseFile("programs\\generated\\test82.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase83 = {
    val program = parser.parseFile("programs\\generated\\test83.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase84 = {
    val program = parser.parseFile("programs\\generated\\test84.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase85 = {
    val program = parser.parseFile("programs\\generated\\test85.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase86 = {
    val program = parser.parseFile("programs\\generated\\test86.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase87 = {
    val program = parser.parseFile("programs\\generated\\test87.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase88 = {
    val program = parser.parseFile("programs\\generated\\test88.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase89 = {
    val program = parser.parseFile("programs\\generated\\test89.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase90 = {
    val program = parser.parseFile("programs\\generated\\test90.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase91 = {
    val program = parser.parseFile("programs\\generated\\test91.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase92 = {
    val program = parser.parseFile("programs\\generated\\test92.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase93 = {
    val program = parser.parseFile("programs\\generated\\test93.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase94 = {
    val program = parser.parseFile("programs\\generated\\test94.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase95 = {
    val program = parser.parseFile("programs\\generated\\test95.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase96 = {
    val program = parser.parseFile("programs\\generated\\test96.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase97 = {
    val program = parser.parseFile("programs\\generated\\test97.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase98 = {
    val program = parser.parseFile("programs\\generated\\test98.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase99 = {
    val program = parser.parseFile("programs\\generated\\test99.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }
}