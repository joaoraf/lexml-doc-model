package br.gov.lexml.doc.test

import java.io.File
import scala.xml._
import br.gov.lexml.schema.scala.LexmlSchema
import br.gov.lexml.doc.xml.XmlConverter
import br.gov.lexml.doc._
import scala.collection.Seq

object Test1 extends App {
  
  val samplesDir = new File("/home/joao/workspace_oxygen2/lexml-schema-scala/src/test/samples")
  val samples = samplesDir.listFiles().filter(_.getName.endsWith(".xml")).to[Seq]
  val scalaxbElems = samples.flatMap { f =>
    try { Seq((f,LexmlSchema(f))) } catch { case _ : Exception => Seq() }
  }
  val elems = scalaxbElems.flatMap { case (f,e) =>
    try {
      Seq((f,XmlConverter.scalaxbToModel(e)))
    } catch {
      case ex : Exception =>
        System.err.println("On " + f + ":")
        ex.printStackTrace()
        Seq()
    }
  }
  
  println("Result: ")
  elems.foreach { case (f,x) =>
    println("File: " + f + " Value: ")
    println(PP.prettyPrint(x, 2, 120, 0))
  }
  
}