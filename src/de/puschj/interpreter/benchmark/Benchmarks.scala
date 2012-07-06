package de.puschj.interpreter.benchmark

import de.puschj.interpreter._
import de.puschj.interpreter.ProgramUtils._
import de.puschj.interpreter.test.TestConstraints._
import de.puschj.parser.WhileParser
import scala.collection.mutable.{Map => MMap}
import de.fosd.typechef.conditional.ConditionalLib
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.featureexpr.FeatureExprFactory
import scala.collection.mutable.ListBuffer

case class BenchmarkResult(nFeatures: Int, nFeatureExpressions: Int, nVariants: Int, nStatements: Int, tVAInterpreter: Long, tBFInterpreter: Long)

class ResultSeries(private val results: List[BenchmarkResult]) {
    def saveToFile() = {
       val time = new java.util.Date().getTime
       val sb = new StringBuilder()
       sb.append("Features;FeatureExpressions;Variants;Statements;VAInterpreter;BruteForce\n")
       for (res <- results) {
           sb.append(res.nFeatures.toString)
           sb.append(";")
           sb.append(res.nFeatureExpressions.toString)
           sb.append(";")
           sb.append(res.nVariants.toString)
           sb.append(";")
           sb.append(res.nStatements.toString)
           sb.append(";")
           sb.append(res.tVAInterpreter.toString)
           sb.append(";")
           sb.append(res.tBFInterpreter.toString)
           sb.append("\n")
       }
       FileUtils.saveToFile("benchmarks\\benchmark"+time+".csv", sb.toString)
    }
    def getResults() = results
}

class CompoundResultSeries() {
    private val resultseries = ListBuffer.empty[ResultSeries]
    
    def addSeries(series: ResultSeries) = {
      if (series.getResults.isEmpty)
        throw new IllegalArgumentException("Series must have at least one Result.")
      if (resultseries.size > 0)
        if (series.getResults.size != resultseries(0).getResults.size)
          throw new IllegalArgumentException("Series must have the same number of elements.")
      resultseries += series
    }
  
    def saveToFile() = {
       val time = new java.util.Date().getTime
       val sb = new StringBuilder()
       sb.append("Features;Variants;Statements;VAInterpreter;BruteForce\n")
       
       var nResults = 0
       if (!resultseries.isEmpty) nResults = resultseries(0).getResults.size
       
       for (i <- 0 until nResults) {
           var features = 0
           var featureExpressions = 0
           var variants = 0
           var statements = 0
           var VAInterpreter = 0L
           var BFInterpreter = 0L
           val nSeries = resultseries.size
           for (k <- 0 until nSeries) {
               val result = resultseries(k).getResults()(i)
               features += result.nFeatures
               featureExpressions += result.nFeatureExpressions
               variants += result.nVariants
               statements += result.nStatements
               VAInterpreter += result.tVAInterpreter
               BFInterpreter += result.tBFInterpreter
           }
           features /= nSeries
           featureExpressions /= nSeries
           variants /= nSeries
           statements /= nSeries
           VAInterpreter /= nSeries
           BFInterpreter /= nSeries
           sb.append(features.toString)
           sb.append(";")
           sb.append(featureExpressions.toString)
           sb.append(";")
           sb.append(variants.toString)
           sb.append(";")
           sb.append(statements.toString)
           sb.append(";")
           sb.append(VAInterpreter.toString)
           sb.append(";")
           sb.append(BFInterpreter.toString)
           sb.append("\n")
       }
       FileUtils.saveToFile("benchmarks\\benchmarkseries"+time+".csv", sb.toString)
    }
}

object Benchmarks {
  private val parser = new WhileParser()
  
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)
  
  private def programInterpretationBenchmark(program: VariableProgram, availableFeatures: Set[String], comparedVariables: Set[String]): BenchmarkResult = {
      var start = 0L
      var stop = 0L
      var bid = 0L
      var va_bench = 0L
      var bf_bench = 0L
      
      start = System.nanoTime
      val variableStore = program.run(new Store, new FuncStore)
      stop = System.nanoTime
      va_bench = (stop - start)
      
      val allVariantsMap = allProgramVariants(program, availableFeatures)
      for (programVariant <- allVariantsMap) {
        start = System.nanoTime
        val configuredStore = programVariant._1.run(new Store, new FuncStore)
        stop = System.nanoTime
        bf_bench += (stop - start)
      }

      val features = countProgramFeatures(program)
      val featureExpressions = countProgramFeatureExpressions(program)
      val variants = allVariantsMap.size
      val statements = countProgramStatments(program)
      val result = new BenchmarkResult(features, featureExpressions, variants, statements, va_bench, bf_bench)
      return result
    }
  
  def variantsComparison() = {
     val results = ListBuffer.empty[BenchmarkResult]
     for (i <- 0 until 100) {
       results += programInterpretationBenchmark(parser.parseFile("testprograms\\test"+(if (i<10) "0"+i else i)+".txt"), FEATURENAMES.toSet, VARNAMES.toSet)
     }
     new ResultSeries(results.toList)
  }
  
  def main(args: Array[String]) {
    val compoundSeries = new CompoundResultSeries()
    
    // warmup benchmark
    variantsComparison()
    
    for (i <- 0 until 3) {
        val benchmark = variantsComparison()
        compoundSeries.addSeries(benchmark)
        benchmark.saveToFile
    }
    compoundSeries.saveToFile
  }
}
    