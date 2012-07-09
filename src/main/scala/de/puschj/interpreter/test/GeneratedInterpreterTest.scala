package de.puschj.interpreter.test

import de.puschj.interpreter.test.TestConstraints._
import de.fosd.typechef.featureexpr.FeatureExpr
import org.junit._
import Assert._
import de.puschj.parser.WhileParser
import de.puschj.interpreter.VariableProgram
import de.puschj.interpreter.ProgramUtils
import de.fosd.typechef.featureexpr.FeatureExprFactory


class GeneratedInterpreterTest {
  
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  val parser: WhileParser = new WhileParser()
    
  @Test
  def testCase00 = {
    val program = parser.parseFile("testprograms\\test00.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }
  
  @Test
  def testCase01 = {
    val program = parser.parseFile("testprograms\\test01.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase02 = {
    val program = parser.parseFile("testprograms\\test02.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase03 = {
    val program = parser.parseFile("testprograms\\test03.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase04 = {
    val program = parser.parseFile("testprograms\\test04.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase05 = {
    val program = parser.parseFile("testprograms\\test05.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase06 = {
    val program = parser.parseFile("testprograms\\test06.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase07 = {
    val program = parser.parseFile("testprograms\\test07.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase08 = {
    val program = parser.parseFile("testprograms\\test08.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase09 = {
    val program = parser.parseFile("testprograms\\test09.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase10 = {
    val program = parser.parseFile("testprograms\\test10.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase11 = {
    val program = parser.parseFile("testprograms\\test11.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase12 = {
    val program = parser.parseFile("testprograms\\test12.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase13 = {
    val program = parser.parseFile("testprograms\\test13.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase14 = {
    val program = parser.parseFile("testprograms\\test14.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase15 = {
    val program = parser.parseFile("testprograms\\test15.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase16 = {
    val program = parser.parseFile("testprograms\\test16.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase17 = {
    val program = parser.parseFile("testprograms\\test17.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase18 = {
    val program = parser.parseFile("testprograms\\test18.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase19 = {
    val program = parser.parseFile("testprograms\\test19.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase20 = {
    val program = parser.parseFile("testprograms\\test20.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase21 = {
    val program = parser.parseFile("testprograms\\test21.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase22 = {
    val program = parser.parseFile("testprograms\\test22.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase23 = {
    val program = parser.parseFile("testprograms\\test23.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase24 = {
    val program = parser.parseFile("testprograms\\test24.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase25 = {
    val program = parser.parseFile("testprograms\\test25.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase26 = {
    val program = parser.parseFile("testprograms\\test26.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase27 = {
    val program = parser.parseFile("testprograms\\test27.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase28 = {
    val program = parser.parseFile("testprograms\\test28.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase29 = {
    val program = parser.parseFile("testprograms\\test29.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase30 = {
    val program = parser.parseFile("testprograms\\test30.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase31 = {
    val program = parser.parseFile("testprograms\\test31.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase32 = {
    val program = parser.parseFile("testprograms\\test32.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase33 = {
    val program = parser.parseFile("testprograms\\test33.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase34 = {
    val program = parser.parseFile("testprograms\\test34.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase35 = {
    val program = parser.parseFile("testprograms\\test35.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase36 = {
    val program = parser.parseFile("testprograms\\test36.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase37 = {
    val program = parser.parseFile("testprograms\\test37.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase38 = {
    val program = parser.parseFile("testprograms\\test38.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase39 = {
    val program = parser.parseFile("testprograms\\test39.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase40 = {
    val program = parser.parseFile("testprograms\\test40.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase41 = {
    val program = parser.parseFile("testprograms\\test41.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase42 = {
    val program = parser.parseFile("testprograms\\test42.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase43 = {
    val program = parser.parseFile("testprograms\\test43.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase44 = {
    val program = parser.parseFile("testprograms\\test44.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase45 = {
    val program = parser.parseFile("testprograms\\test45.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase46 = {
    val program = parser.parseFile("testprograms\\test46.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase47 = {
    val program = parser.parseFile("testprograms\\test47.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase48 = {
    val program = parser.parseFile("testprograms\\test48.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase49 = {
    val program = parser.parseFile("testprograms\\test49.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase50 = {
    val program = parser.parseFile("testprograms\\test50.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase51 = {
    val program = parser.parseFile("testprograms\\test51.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase52 = {
    val program = parser.parseFile("testprograms\\test52.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase53 = {
    val program = parser.parseFile("testprograms\\test53.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase54 = {
    val program = parser.parseFile("testprograms\\test54.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase55 = {
    val program = parser.parseFile("testprograms\\test55.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase56 = {
    val program = parser.parseFile("testprograms\\test56.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase57 = {
    val program = parser.parseFile("testprograms\\test57.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase58 = {
    val program = parser.parseFile("testprograms\\test58.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase59 = {
    val program = parser.parseFile("testprograms\\test59.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase60 = {
    val program = parser.parseFile("testprograms\\test60.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase61 = {
    val program = parser.parseFile("testprograms\\test61.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase62 = {
    val program = parser.parseFile("testprograms\\test62.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase63 = {
    val program = parser.parseFile("testprograms\\test63.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase64 = {
    val program = parser.parseFile("testprograms\\test64.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase65 = {
    val program = parser.parseFile("testprograms\\test65.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase66 = {
    val program = parser.parseFile("testprograms\\test66.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase67 = {
    val program = parser.parseFile("testprograms\\test67.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase68 = {
    val program = parser.parseFile("testprograms\\test68.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase69 = {
    val program = parser.parseFile("testprograms\\test69.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase70 = {
    val program = parser.parseFile("testprograms\\test70.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase71 = {
    val program = parser.parseFile("testprograms\\test71.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase72 = {
    val program = parser.parseFile("testprograms\\test72.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase73 = {
    val program = parser.parseFile("testprograms\\test73.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase74 = {
    val program = parser.parseFile("testprograms\\test74.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase75 = {
    val program = parser.parseFile("testprograms\\test75.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase76 = {
    val program = parser.parseFile("testprograms\\test76.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase77 = {
    val program = parser.parseFile("testprograms\\test77.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase78 = {
    val program = parser.parseFile("testprograms\\test78.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase79 = {
    val program = parser.parseFile("testprograms\\test79.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase80 = {
    val program = parser.parseFile("testprograms\\test80.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase81 = {
    val program = parser.parseFile("testprograms\\test81.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase82 = {
    val program = parser.parseFile("testprograms\\test82.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase83 = {
    val program = parser.parseFile("testprograms\\test83.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase84 = {
    val program = parser.parseFile("testprograms\\test84.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase85 = {
    val program = parser.parseFile("testprograms\\test85.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase86 = {
    val program = parser.parseFile("testprograms\\test86.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase87 = {
    val program = parser.parseFile("testprograms\\test87.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase88 = {
    val program = parser.parseFile("testprograms\\test88.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase89 = {
    val program = parser.parseFile("testprograms\\test89.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase90 = {
    val program = parser.parseFile("testprograms\\test90.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase91 = {
    val program = parser.parseFile("testprograms\\test91.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase92 = {
    val program = parser.parseFile("testprograms\\test92.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase93 = {
    val program = parser.parseFile("testprograms\\test93.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase94 = {
    val program = parser.parseFile("testprograms\\test94.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase95 = {
    val program = parser.parseFile("testprograms\\test95.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase96 = {
    val program = parser.parseFile("testprograms\\test96.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase97 = {
    val program = parser.parseFile("testprograms\\test97.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase98 = {
    val program = parser.parseFile("testprograms\\test98.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }

  @Test
  def testCase99 = {
    val program = parser.parseFile("testprograms\\test99.txt")
    assertTrue(ProgramUtils.compareProgramVariants(program, FEATURENAMES.toSet, VARNAMES.toSet))
  }
}