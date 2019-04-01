package br.gov.lexml.doc.xml

import br.gov.lexml.schema.scala.{data => X}
import br.gov.lexml.{doc => M}
import br.gov.lexml.doc.LexmlDocument


object XmlConverter {
  
  def scalaxbToModel(md : X.MetaSection) : M.Metadado = {
    M.Metadado(identificacao = M.Identificacao(M.LexMLURN(md.Identificacao.URN)))    
  }
  
  def scalaxbToModel(dt : X.DocumentTypes) : M.LexmlDocument = {
    val md = scalaxbToModel(dt.Metadado)
    val contents = (dt.documenttypesoption.key,dt.documenttypesoption.value) match {
      case (_,pj : X.ProjetoNorma) =>
        scalaxbToModel(pj)
      case (Some("Norma"),norma : X.HierarchicalStructure) =>
        M.Norma(scalaxbToModel(norma))
      case (key,value) =>
        sys.error(s"Tipo de documento não suportado. Label = ${key}, conteudo = ${value}")
    }
    LexmlDocument(md,contents)
  }
  
  def scalaxbToModel(pj : X.ProjetoNorma) : M.ProjetoNorma = {
    M.ProjetoNorma(M.Norma(scalaxbToModel(pj.Norma)))    
  }
  
  def scalaxbToModel(in : X.Inlinable) : M.InlineElement = ??? 
  
  def scalaxbToModel(hs : X.HierarchicalStructure) : M.HierarchicalStructure = {    
    hs.ParteInicial.map { pi =>
      val ementa = pi.Ementa.map(x => M.Ementa(scalaxbToModel(x)))
      val formulaPromulgacao = pi.FormulaPromulgacao.flatMap(x =>
          x.p.map { i =>
            M.FormulaPromulgacao(M.InlineSeq(Seq(scalaxbToModel(i)))) }
          )
      
      M.HierarchicalStructure(
          ementa = ementa)
    }
    ???
  }
  
  def scalaxbToModel(inlineReq : X.InlineReq) : M.InlineSeq = ???
}