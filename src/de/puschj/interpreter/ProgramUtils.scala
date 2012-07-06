package de.puschj.interpreter

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import scala.collection.mutable.{HashSet => MHashSet}
import de.puschj.parser.WhileParser
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.conditional.ConditionalLib
import java.text.SimpleDateFormat
import de.puschj.interpreter.benchmark.BenchmarkResult
import de.fosd.typechef.conditional.Opt


object ProgramUtils {
  
    private def allCombinations[T](list: List[T]) = {
      val result = ListBuffer.empty[Set[T]]
      result.append(Set.empty[T])
      for (i <- 1 to list.length)
          for (x <- list.combinations(i))
              result.append(x.toSet)
      result.toList
    }
    
    def countProgramStatments(program: VariableProgram) = countStatements(program.getStatements)
    
    private def countStatements(stmts: List[Opt[Statement]]): Int = {
      var c = 0
      for (optStmt <- stmts) {
        val stmt = optStmt.entry
        stmt match {
          case Block(body) => c += countStatements(body)
          case While(_, Block(body)) => c += (countStatements(body) + 1)
          case If(_, Block(thn), None) => c += (countStatements(thn) + 1)
          case If(_, Block(thn), Some(Block(els))) => c += (countStatements(thn) + countStatements(els) + 1) 
          case s => c += 1
        }
      }
      return c
    }
    
    def countProgramFeatures(program: VariableProgram) = countFeatures(program.getStatements)
    
    private def countFeatures(stmts: List[Opt[Statement]]) = {
      val features = distinctFeatures(stmts)
      features.size
    }
    
    private def distinctFeatures(stmts: List[Opt[Statement]]): MSet[String] = {
      var features = MHashSet.empty[String]
      for (optStmt <- stmts) {
        features ++= optStmt.feature.collectDistinctFeatures
        val stmt = optStmt.entry
        stmt match {
          case Block(body) => features ++= distinctFeatures(body)
          case While(_, Block(body)) => features ++= distinctFeatures(body)
          case If(_, Block(thn), None) => features ++= distinctFeatures(thn)
          case If(_, Block(thn), Some(Block(els))) => features ++= distinctFeatures(thn) ++= distinctFeatures(els)
          case s => 
        }      
      }
      features
    }
    
    def countProgramFeatureExpressions(program: VariableProgram) = countFeatures(program.getStatements)
    
    private def countFeatureExpressions(stmts: List[Opt[Statement]]): Int = {
      var features = 0
      for (optStmt <- stmts) {
        val feat = optStmt.feature
        val stmt = optStmt.entry
        stmt match {
          case Block(body) => features += countFeatures(body)
          case While(_, Block(body)) => features += countFeatures(body)
          case If(_, Block(thn), None) => features += countFeatures(thn)
          case If(_, Block(thn), Some(Block(els))) => features += (countFeatures(thn) + countFeatures(els))
          case s => 
        }
        if (feat != True) features += 1
      }
      features
    }
    
    def allProgramVariants(program: VariableProgram, availableFeatures: Set[String]) = {
        val mmap = MMap.empty[ConfiguredProgram, List[Set[String]]]
        for (selectedFeatures <- allCombinations(availableFeatures.toList)) {
          val configuredProgram = program.configured(selectedFeatures)
          val entry = mmap.get(configuredProgram)
          if (!entry.isDefined)
            mmap.put(configuredProgram ,List(selectedFeatures))
          else
            mmap.put(configuredProgram, selectedFeatures :: entry.get)
        }
        mmap.toMap
    }
    
    def compareProgramVariants(program: VariableProgram, availableFeatures: Set[String], comparedVariables: Set[String]): Boolean = {
      val variableStore = program.run(new Store, new FuncStore)
      val allVariantsMap = allProgramVariants(program, availableFeatures)
      for (programVariant <- allVariantsMap) {
        val configuredStore = programVariant._1.run(new Store, new FuncStore)
        for (selectedFeatures <- programVariant._2) {
           var context = True
           context = selectedFeatures.foldLeft(context)( (f, s) => f and createDefinedExternal(s))
           context = (availableFeatures -- selectedFeatures).foldLeft(context)( (f, s) => f andNot createDefinedExternal(s))
           for (variable <- comparedVariables) {
             val varStoreValue = variableStore.getByContext(variable, context)
             val cnfStoreValue = configuredStore.get(variable)
             if (!ConditionalLib.equals(varStoreValue, cnfStoreValue)) {
               System.err.println("Different Values for \""+variable+"\" - expected: "+cnfStoreValue+" result: "+varStoreValue);
//               System.err.println("==== Variable Program: ====\n"+program.toString+"\n");
//               System.err.println("=== Configured Program: ===\n"+programVariant._1.toString);
               return false
             }
           }
        }
      }
      return true
    }
    
}