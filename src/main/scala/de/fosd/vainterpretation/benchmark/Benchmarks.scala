package de.fosd.vainterpretation.benchmark

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.vainterpretation.interpreter.ProgramUtils._
import de.fosd.vainterpretation.interpreter.test.TestConstraints.FEATURENAMES
import de.fosd.vainterpretation.interpreter.test.TestConstraints.VARNAMES
import de.fosd.vainterpretation.interpreter.FileUtils
import de.fosd.vainterpretation.interpreter.VariableProgram
import de.fosd.vainterpretation.parser.WhileParser
import java.util.Locale

case class BenchmarkResult {
  val attributes = HashMap.empty[String, Double]
  
  def put(key: String, value: Double) = attributes.put(key, value)
  
  def get(key: String) = attributes.get(key)
  
  def keySet = attributes.keySet
  
  def valueSet = attributes.values
  
  def attributeHeader() = {
    val sb = new StringBuilder
    for (a <- attributes) {
      sb.append(a._1)
      sb.append(";")
    }
    sb.deleteCharAt(sb.length - 1)
    sb.toString
  }
  
  override def toString() = {
    val sb = new StringBuilder
    sb.append(attributeHeader())
    sb.append("\n")
    for (a <- attributes) {
      val s = String.format("%.2f", a._2.asInstanceOf[java.lang.Object])
//      sb.append(" " * (a._1.size - s.size))
      sb.append(s)
      sb.append(";")
    }
    sb.deleteCharAt(sb.size - 1)
    sb.toString
  }
}
                           

                           

class ResultList() {
    private val results = ListBuffer.empty[BenchmarkResult]
    
    def add(br: BenchmarkResult): Unit = {
      results += br
    }
    
    def getResults = results
    
    def +=(br: BenchmarkResult) = add(br)
    
    override def toString(): String = {
      if (results.isEmpty) return ""
      val head = results.head
      val sb = new StringBuilder
      sb.append(head.attributeHeader)
      sb.append("\n")
      
      val attributes = head.keySet
      val size = results.size
      
      for (i <- 0 until size) {
        for (a <- attributes) {
          val value = results(i).get(a)
          sb.append(if (value.isDefined) value.get else "")
          sb.append(";")
        }
        sb.deleteCharAt(sb.length - 1)
        sb.append("\n")
      }
      sb.toString
    }
    
    def saveToFile(name: String = "benchmark") = {
       val time = new java.util.Date().getTime
       FileUtils.saveToFile("benchmarks\\"+name+time+".csv", toString())
    }
}

class CompoundResultSeries() {
    private val resultseries = ListBuffer.empty[ResultList]
    
    def addSeries(list: ResultList): Unit = {
      resultseries += list
    }
    
    def +=(list: ResultList) = addSeries(list)
    
    override def toString(): String = {
      if (resultseries.isEmpty) return ""
      val sb = new StringBuilder
      sb.append(resultseries.head.getResults.head.attributeHeader)
      sb.append("\n")
      
      val attributes = resultseries.head.getResults.head.keySet
      val nResults = resultseries.head.getResults.size
      val nResultLists = resultseries.size
      
      for (i <- 0 until nResults) {
        for (a <- attributes) {
          var value = 0.0
          for (k <- 0 until nResultLists) {
              value += resultseries(k).getResults(i).get(a).get
          }
          value /= nResultLists
          sb.append(value)
          sb.append(";")
        }
        sb.deleteCharAt(sb.length - 1)
        sb.append("\n")
      }
      sb.toString
    }
  
    def saveToFile(name: String = "benchmarkseries") = {
       val time = new java.util.Date().getTime
       FileUtils.saveToFile("benchmarks\\"+name+time+".csv", toString)
    }
}

object Benchmarks {
  private val parser = new WhileParser()
  
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  
  private def programInterpretationBenchmark(program: VariableProgram, availableFeatures: Set[String]): BenchmarkResult = {
      var start = 0L
      var stop = 0L
      var va_bench = 0L
      var bf_bench = 0L
      
      start = System.nanoTime
      program.run()
      stop = System.nanoTime
      va_bench = (stop - start)
      
      val allVariantsMap = allProgramVariants(program, availableFeatures)
      for (programVariant <- allVariantsMap) {
        start = System.nanoTime
        programVariant._1.run()
        stop = System.nanoTime
        bf_bench += (stop - start)
      }
      
      val result = new BenchmarkResult
      result.put("Features", countProgramFeatures(program))
      result.put("FeatureExpressions", countProgramFeatureExpressions(program))
      result.put("AndFeatureExpressions", countAndFeatureExpressions(program))
//      result.put("FeatureLength", countFeatureLength(program))
      result.put("Variants", allVariantsMap.size)
      result.put("Statements", countProgramStatments(program))
      result.put("Assigns", countAssignStatments(program))
      result.put("Whiles", countWhileStatments(program))
      result.put("Ifs", countIfStatments(program))
//      result.put("WhilesWithExpr", countWhileStatementsWithVariability(program))
//      result.put("NestedWhiles", countNestedWhileLoops(program))
//      result.put("WhileIterations", countWhileStatementIterations(program))
      result.put("VAInterpreter", va_bench)
      result.put("BruteForce", bf_bench)
      result.put("Speedup", bf_bench / va_bench.asInstanceOf[Double])
      return result
    }
  
  private val FEATURES10 = Set("A","B","C","D","E","F","G","H","I","J")
  
  def benchmarkEarlyJoining() = {
    val resultList = new ResultList
     for (i <- 0 to 10) {
       resultList += programInterpretationBenchmark(parser.parseFile("programs\\earlyjoining\\ej"+(if (i<10) "0"+i else i)+".txt"), FEATURES10)
       System.out.print("|")
     }
     println()
     resultList
  }
  
  def benchmarkLateSplitting() = {
    val resultList = new ResultList
     for (i <- 0 to 10) {
       resultList += programInterpretationBenchmark(parser.parseFile("programs\\latesplitting\\ls"+(if (i<10) "0"+i else i)+".txt"), FEATURES10)
       System.out.print("|")
     }
     println()
     resultList
  }
  
  def benchmarkTestprogram() = {
    val program = parser.parseFile("program_test.txt")
    val result = programInterpretationBenchmark(program, FEATURES10)
    result
  }
  
  def benchmarkGeneratedTestprograms() = {
     val resultList = new ResultList
     for (i <- 0 until 100) {
       resultList += programInterpretationBenchmark(parser.parseFile("programs\\generated\\"+(if (i<10) "0"+i else i)+".txt"), FEATURENAMES.toSet)
       System.out.print("|")
     }
     println()
     resultList
  }
  
  def main(args: Array[String]) {
    val compoundSeries = new CompoundResultSeries()
    
//  warmup benchmark
//    benchmarkGeneratedTestprograms()
    
    for (i <- 0 until 3) {
        println("test "+i+" progress:  ")
        compoundSeries.addSeries(benchmarkGeneratedTestprograms)
        println("finished test "+i)
    }
//    compoundSeries.saveToFile("TestSeries")
    println(compoundSeries)
  }
}
    