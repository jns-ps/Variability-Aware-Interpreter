package de.fosd.vainterpretation.interpreter

import scala.collection.mutable.{HashSet => MHashSet}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}
import scala.collection.mutable.{Set => MSet}
import de.fosd.typechef.conditional.One
import de.fosd.typechef.conditional.Opt
import de.fosd.typechef.featureexpr.bdd.BDDFeatureExpr
import de.fosd.typechef.featureexpr.bdd.{True => BDDTrue}
import de.fosd.typechef.featureexpr.FeatureExprFactory.True
import de.fosd.typechef.featureexpr.FeatureExprFactory.createDefinedExternal
import net.sf.javabdd.BDD



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
    
    private def countStatements(stmts: List[Opt[Stmt]]): Int = {
      var c = 0
      for (Opt(_, stmt) <- stmts) {
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
    
    def countAssignStatments(program: VariableProgram) = countAssigns(program.getStatements)
    
    private def countAssigns(stmts: List[Opt[Stmt]]): Int = {
      var c = 0
      for (Opt(_, stmt) <- stmts) {
        stmt match {
          case Block(body) => c += countAssigns(body)
          case While(_, Block(body)) => c += (countAssigns(body))
          case If(_, Block(thn), None) => c += (countAssigns(thn))
          case If(_, Block(thn), Some(Block(els))) => c += (countAssigns(thn) + countAssigns(els)) 
          case a: Assign => c += 1
          case s =>
        }
      }
      return c
    }
    
    def countWhileStatments(program: VariableProgram) = countWhiles(program.getStatements)
    
    private def countWhiles(stmts: List[Opt[Stmt]]): Int = {
      var c = 0
      for (Opt(_, stmt) <- stmts) {
        stmt match {
          case Block(body) => c += countWhiles(body)
          case While(_, Block(body)) => c += (countWhiles(body) + 1)
          case If(_, Block(thn), None) => c += (countWhiles(thn) )
          case If(_, Block(thn), Some(Block(els))) => c += (countWhiles(thn) + countWhiles(els)) 
          case s =>
        }
      }
      return c
    }
    
    def countWhileStatementIterations(program: VariableProgram) = countWhileIterations(program.getStatements)
    
    private def countWhileIterations(stmts: List[Opt[Stmt]]): Int = {
      var c = 0
      for (Opt(_, stmt) <- stmts) {
        stmt match {
          case Block(body) => c += countWhileIterations(body)
          case While(One(cond), Block(body)) => {
              val iterations = cond.asInstanceOf[LeT].e2.asInstanceOf[Num].n
              val bodyIterations = countWhileIterations(body)
              c += (scala.math.max(bodyIterations,1) * iterations) 
          }
          case If(_, Block(thn), None) => c += (countWhileIterations(thn) )
          case If(_, Block(thn), Some(Block(els))) => c += (countWhileIterations(thn) + countWhileIterations(els)) 
          case s =>
        }
      }
      return c
    }
    
    def countNestedWhileLoops(program: VariableProgram) = countNestedWhiles(program.getStatements, 0)
    
    private def countNestedWhiles(stmts: List[Opt[Stmt]], nested: Int): Int = {
      var c = 0
      for (Opt(_, stmt) <- stmts) {
        stmt match {
          case Block(body) => c += countNestedWhiles(body, nested)
          case While(_, Block(body)) => c += (countNestedWhiles(body, nested + 1) + (if (nested > 0) 1 else 0))
          case If(_, Block(thn), None) => c += (countNestedWhiles(thn, nested) )
          case If(_, Block(thn), Some(Block(els))) => c += (countNestedWhiles(thn, nested) + countNestedWhiles(els, nested)) 
          case s =>
        }
      }
      return c
    }
    
    def countWhileStatementsWithVariability(program: VariableProgram) = countWhilesWithVariability(program.getStatements)
    
    private def countWhilesWithVariability(stmts: List[Opt[Stmt]]): Int = {
      var c = 0
      for (Opt(feature, stmt) <- stmts) {
        stmt match {
          case Block(body) => c += countWhilesWithVariability(body)
          case While(_, Block(body)) => c += (countWhilesWithVariability(body) + (if (!feature.isTautology) 1 else 0))
          case If(_, Block(thn), None) => c += (countWhilesWithVariability(thn) )
          case If(_, Block(thn), Some(Block(els))) => c += (countWhilesWithVariability(thn) + countWhilesWithVariability(els)) 
          case s =>
        }
      }
      return c
    }
    
    def countIfStatments(program: VariableProgram) = countIfs(program.getStatements)
    
    private def countIfs(stmts: List[Opt[Stmt]]): Int = {
      var c = 0
      for (Opt(_, stmt) <- stmts) {
        stmt match {
          case Block(body) => c += countIfs(body)
          case While(_, Block(body)) => c += (countIfs(body))
          case If(_, Block(thn), None) => c += (countIfs(thn) + 1)
          case If(_, Block(thn), Some(Block(els))) => c += (countIfs(thn) + countIfs(els) + 1) 
          case s =>
        }
      }
      return c
    }
    
    def countProgramFeatures(program: VariableProgram) = countFeatures(program.getStatements)
    
    private def countFeatures(stmts: List[Opt[Stmt]]) = {
      val features = distinctFeatures(stmts)
      features.size
    }
    
    def distinctFeatures(stmts: List[Opt[Stmt]]): Set[String] = {
      var features = MHashSet.empty[String]
      for (Opt(feature, stmt) <- stmts) {
        features ++= feature.collectDistinctFeatures
        stmt match {
          case Block(body) => features ++= distinctFeatures(body)
          case While(_, Block(body)) => features ++= distinctFeatures(body)
          case If(_, Block(thn), None) => features ++= distinctFeatures(thn)
          case If(_, Block(thn), Some(Block(els))) => features ++= (distinctFeatures(thn) ++ distinctFeatures(els))
          case s => 
        }      
      }
      features.toSet
    }
    
    def countProgramFeatureExpressions(program: VariableProgram) = countFeatureExpressions(program.getStatements)
    
    private def countFeatureExpressions(stmts: List[Opt[Stmt]]): Int = {
      var features = 0
      for (Opt(feat, stmt) <- stmts) {
        stmt match {
          case Block(body) => features += countFeatureExpressions(body)
          case While(_, Block(body)) => features += countFeatureExpressions(body)
          case If(_, Block(thn), None) => features += countFeatureExpressions(thn)
          case If(_, Block(thn), Some(Block(els))) => features += (countFeatureExpressions(thn) + countFeatureExpressions(els))
          case s => 
        }
        if (feat != True) features += 1
      }
      features
    }
    
    def countAndFeatureExpressions(program: VariableProgram) = countAnds(program.getStatements)
    
    private def countAnds(stmts: List[Opt[Stmt]]): Int = {
      var features = 0
      for (Opt(feat, stmt) <- stmts) {
        stmt match {
          case Block(body) => features += countAnds(body)
          case While(_, Block(body)) => features += countAnds(body)
          case If(_, Block(thn), None) => features += countAnds(thn)
          case If(_, Block(thn), Some(Block(els))) => features += (countAnds(thn) + countAnds(els))
          case s => 
        }
        features += feat.toString.count(_ == '&')
      }
      features
    }
    
    def countFeatureLength(program: VariableProgram): Int = featureLength(program.getStatements)
    
    private def featureLength(stmts: List[Opt[Stmt]]): Int = {
      var flength = 0
      for (Opt(feat, stmt) <- stmts) {
        stmt match {
          case Block(body) => flength += featureLength(body)
          case While(_, Block(body)) => flength += featureLength(body)
          case If(_, Block(thn), None) => flength += featureLength(thn)
          case If(_, Block(thn), Some(Block(els))) => flength += (featureLength(thn) + featureLength(els))
          case s => 
        }
        val string = feat.toString
        val length = string.count(_ match {
              case 'A' => true
              case 'B' => true
              case 'C' => true
              case 'D' => true
              case 'E' => true
              case 'F' => true
              case c => false
            })
        flength += length//scala.math.pow(length, 2).asInstanceOf[Int]
            
      }
      flength
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
      val variableStore = program.run()
      val allVariantsMap = allProgramVariants(program, availableFeatures)
      for (programVariant <- allVariantsMap) {
        val configuredStore = programVariant._1.run()
        for (selectedFeatures <- programVariant._2) {
           var context = True
           context = selectedFeatures.foldLeft(context)( (f, s) => f and createDefinedExternal(s))
           context = (availableFeatures -- selectedFeatures).foldLeft(context)( (f, s) => f andNot createDefinedExternal(s))
           for (variable <- comparedVariables) {
             val varStoreValue = variableStore.getByContext(variable, context).asInstanceOf[One[Value]].value
             val cnfStoreValue = configuredStore.get(variable)
             if (!(varStoreValue equals cnfStoreValue)) {
               System.err.println("Error in configuration: "+selectedFeatures);
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