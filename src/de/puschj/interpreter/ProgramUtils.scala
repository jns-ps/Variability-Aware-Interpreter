package de.puschj.interpreter
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MMap}
import de.puschj.parser.WhileParser
import de.fosd.typechef.featureexpr.FeatureExprFactory
import de.fosd.typechef.featureexpr.FeatureExprFactory._
import de.fosd.typechef.conditional.ConditionalLib


object ProgramUtils {
  
    private def allCombinations[T](list: List[T]) = {
      val result = ListBuffer.empty[Set[T]]
      result.append(Set.empty[T])
      for (i <- 1 to list.length)
          for (x <- list.combinations(i))
              result.append(x.toSet)
      result.toList
    }
    
    private def allProgramVariants(program: VariableProgram, availableFeatures: Set[String]) = {
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
      val configuredStores = MMap.empty[Store, List[Set[String]]]
      
      for (programVariant <- allProgramVariants(program, availableFeatures)) {
        val configuredStore = programVariant._1.run(new Store, new FuncStore)
        for (selectedFeatures <- programVariant._2) {
           var context = True
           context = selectedFeatures.foldLeft(context)( (f, s) => f and createDefinedExternal(s))
           context = (availableFeatures -- selectedFeatures).foldLeft(context)( (f, s) => f andNot createDefinedExternal(s))
           for (variable <- comparedVariables) {
             val varStoreValue = variableStore.getByContext(variable, context)
             val cnfStoreValue = configuredStore.get(variable)
//             println("comparing "+varStoreValue+"/"+cnfStoreValue)
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