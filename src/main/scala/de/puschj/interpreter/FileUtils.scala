package de.puschj.interpreter

import java.io.File

object FileUtils {

  def saveToFile(filename: String, text: String) = {
    val pw = new java.io.PrintWriter(new File(filename))
    try {
      pw.print(text)
    } 
    finally { 
      pw.close()
    }
  }
    
  def saveProgram(p: Program, filename: String) = saveToFile(filename, p.toString)
  
  def saveProgramAST(p: Program, filename: String) = saveToFile(filename, p.toStringAST)
}