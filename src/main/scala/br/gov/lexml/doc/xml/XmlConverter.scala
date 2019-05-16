package br.gov.lexml.doc.xml

import br.gov.lexml.schema.scala.{data => X}
import br.gov.lexml.{doc => M}
import java.net.URI
import br.gov.lexml.doc.TituloDispositivo
import scala.language.existentials
import org.slf4j.LoggerFactory
import br.gov.lexml.schema.scala.scalaxb

object XmlConverter {
  val logger = LoggerFactory.getLogger(this.getClass)
  
  def scalaxbToModel(md : X.Metadado) : M.Metadado = {
    M.Metadado(identificacao = M.Identificacao(M.LexMLURN(md.Identificacao.URN)))    
  }
  
  def scalaxbToModel(dt : X.LexML) : M.LexmlDocument = {
    val md = scalaxbToModel(dt.Metadado)    
    val co = dt.documentContentOption2
    val contents = (co.key,co.value) match {
      case (_,pj : X.ProjetoNorma) =>
        scalaxbToModel(pj)
      case (Some("Norma"),norma : X.HierarchicalStructure) =>
        M.Norma(scalaxbToModel(norma))
      case (key,value) =>
        sys.error(s"Tipo de documento não suportado. Label = ${key}, conteudo = ${value}")
    }
    M.LexmlDocument(md,contents)
  }
  
  def scalaxbToModel(pj : X.ProjetoNorma) : M.ProjetoNorma = {
    M.ProjetoNorma(M.Norma(scalaxbToModel(pj.Norma)))    
  }
       
  def scalaxbToModel(in : X.TextoTypable) : Seq[M.InlineSeq] = in.p.map(scalaxbToModel)      
  
  def scalaxbToModel(hs : X.HierarchicalStructure) : M.HierarchicalStructure = {
    val articulacao = M.Articulacao(hs.Articulacao.lXhierOption1.map(x => scalaxbToModel(x.key,x.value) : M.HierarchicalElement))
    val res1 = M.HierarchicalStructure(articulacao = articulacao)
    val res2 = hs.ParteInicial.map { pi =>
      val ementa = pi.Ementa.map(x => 
        M.Ementa(
            scalaxbToModel(x),
            abreAspas = x.abreAspas == Some(X.S),
            fechaAspas = x.fechaAspas == Some(X.S),
            notaAlteracao = x.notaAlteracao))
      val formulaPromulgacao = pi.FormulaPromulgacao.map(x =>
            M.FormulaPromulgacao(
                x.p.map(scalaxbToModel).map(y => M.Paragraph(inlineSeq = y))
                ) )
          
      val epigrafe = pi.Epigrafe.map(x => M.Epigrafe(
          scalaxbToModel(x),
          abreAspas = x.abreAspas == Some(X.S),
          fechaAspas = x.fechaAspas == Some(X.S),
          notaAlteracao = x.notaAlteracao))
      val preambulo = pi.Preambulo.map(x => 
        M.Preambulo(
            scalaxbToModel(x).map(M.PreambuloLine),            
            abreAspas = x.abreAspas == Some(X.S),
            fechaAspas = x.fechaAspas == Some(X.S),
            notaAlteracao = x.notaAlteracao))
       
      res1.copy(          
          formulaPromulgacao = formulaPromulgacao,
          epigrafe = epigrafe,
          ementa = ementa,
          preambulo = preambulo
          )
    }.getOrElse(res1)
    val res3 = hs.ParteFinal.map { pf =>
      val ldf = pf.LocalDataFecho.map { pars => M.LocalDataFecho(pars.p.map(scalaxbToModel).map(y => M.Paragraph(inlineSeq = y))) }      
      val assinaturas : Seq[M.Assinatura[_]] = pf.parteFinalAssinaturaOption2.collect {
        case scalaxb.DataRecord(_,Some("AssinaturaTexto"),pars : X.ParsType) => Seq(M.AssinaturaTexto(pars.p.map(scalaxbToModel).map(y => M.Paragraph(inlineSeq = y))))
        case scalaxb.DataRecord(ns,label,data) => 
          logger.warn(s"Ignorando assinatura: ns=${ns}, label=${label}, data=${data}")
          Seq()
      }.flatten      
      res2.copy(localDataFecho = ldf,assinaturas = assinaturas)
    }.getOrElse(res2)
    res3
  }
  
  def scalaxbToModel(in : X.Omissis) : M.Omissis = 
    M.Omissis(
        id = strToID(in.id),
        abreAspas = in.abreAspas == Some(X.S),
        fechaAspas = in.fechaAspas == Some(X.S),
        notaAlteracao = in.notaAlteracao
        )
  
  val tiposAgrupador = Map[String,M.TipoAgrupador](
    "Parte" -> M.TAP_Parte, 
    "Livro" -> M.TAP_Livro,
    "Titulo" -> M.TAP_Titulo,
    "Capitulo" -> M.TAP_Capitulo,
    "Secao" -> M.TAP_Secao,
    "Subsecao" -> M.TAP_Subsecao,
    "AgrupamentoHierarquico" -> M.TA_Generico
      )
      
  def strToID(in : String) : M.ID = M.ID(URI.create(in))
  
  def strToLexMLURN(in : String) : M.LexMLURN = M.LexMLURN(URI.create(in))
  
  def scalaxbToModel(label : String, in : X.Hierarchy) : M.HierarchicalElement =  {        
    val tipo = tiposAgrupador.getOrElse(label,sys.error("Unexpected label for tiposAgrupador: " + label))
    val id = strToID(in.id.getOrElse(sys.error("Expecting id in " + ((label,in)))))
    val rotulo = in.Rotulo.map(M.Rotulo)
    val nomeAgrupador = in.NomeAgrupador.map(x => M.NomeAgrupador(scalaxbToModel(x)))
    val elems = in.lXhierOption3.map(x => scalaxbToModel(x.key,x.value).asInstanceOf[M.HierarchicalElement])
    val abreAspas = in.abreAspas == Some(X.S)
    val fechaAspas = in.fechaAspas == Some(X.S)
    val notaAlteracao = in.notaAlteracao
    tipo match {
      case t : M.TipoAgrupadorPredef =>
        M.AgrupadorPredef(
            tipoAgrupador = t,
            id = id,
            rotulo = rotulo,
            nomeAgrupador = nomeAgrupador,
            elems = elems, 
            abreAspas = abreAspas, 
            fechaAspas = fechaAspas,
            notaAlteracao = notaAlteracao)
      case _ => 
        M.AgrupadorGenerico(
            nome = in.nome.getOrElse(sys.error("Missing attribute 'nome' in (" + label +") " + in)),
            id = id,
            rotulo = rotulo,
            nomeAgrupador = nomeAgrupador,
            elems = elems, 
            abreAspas = abreAspas, 
            fechaAspas = fechaAspas,            
            notaAlteracao = notaAlteracao
            )
    }    
  }
  
  val tiposDispositivo = Map[String,M.TipoDispositivo](
    "Artigo" -> M.TDP_Artigo,
    "Caput" -> M.TDP_Caput,
    "Paragrafo" -> M.TDP_Paragrafo,
    "Inciso" -> M.TDP_Inciso,
    "Alinea" -> M.TDP_Alinea,
    "Item" -> M.TDP_Item,
    "Pena" -> M.TDP_Pena,
    "DispositivoGenerico" -> M.TD_Generico )
  
  def scalaxbToModel1(label : String, in : X.DispositivoType) : M.Dispositivo =  { 
    val tipo = tiposDispositivo.getOrElse(label, sys.error("Unexpected label in tiposDispositivo: " + ((label,in)))) ;                   
    val id = strToID(in.id.getOrElse(sys.error("Expecting id in " + ((label,in)))))
    val rotulo = in.Rotulo.map(M.Rotulo)
    val titulo = in.TituloDispositivo.map(x => M.TituloDispositivo(scalaxbToModel(x)))
    val abreAspas = in.abreAspas == Some(X.S)
    val fechaAspas = in.fechaAspas == Some(X.S)
    val notaAlteracao = in.notaAlteracao
    val conteudo : Option[M.ConteudoDispositivo] = if(in.textoOmitido == Some(X.S)) { 
        Some(M.OmissisSimples) 
      } else if(!in.p.isEmpty) { 
        Some(M.TextoDispositivo(in.p.map(x => M.ParagrafoTextoDispositivo(scalaxbToModel(x))))) 
      } else {
        None
      }
    val alteracao : Option[M.Alteracao] = in.Alteracao.map(scalaxbToModel)

    val containers : Seq[M.LXContainer] = in.lXcontainersOmissisOption5.map(d => (d.key,d.value)).collect {
      case (_,x : X.Omissis) => scalaxbToModel(x) : M.LXContainer 
      case (Some(l),x : X.DispositivoType) => scalaxbToModel1(l,x) : M.Dispositivo
      case (None,x) => sys.error("Expected label at " + x)
    }
    
    tipo match {
      case t :  M.TipoDispositivoNaoArtigo with M.TipoDispositivoPredef => 
        M.DispositivoPredefNA(
          id = id, 
          tipoDispositivo = t,
          titulo = titulo, 
          rotulo = rotulo, 
          conteudo = conteudo, 
          alteracao = alteracao, 
          containers = containers, abreAspas, fechaAspas, notaAlteracao)
      case M.TD_Generico => 
        M.DispositivoGenerico(
          id = id,           
          titulo = titulo, 
          rotulo = rotulo, 
          conteudo = conteudo, 
          alteracao = alteracao, 
          containers = containers, abreAspas, fechaAspas, notaAlteracao)
      case M.TDP_Artigo => 
        M.Artigo(
          id = id,           
          titulo = titulo, 
          rotulo = rotulo, 
          conteudo = conteudo, 
          alteracao = alteracao, 
          containers = containers, abreAspas, fechaAspas, notaAlteracao)      
    } 
  }  
  
  def scalaxbToModel(in : X.Alteracao) : M.Alteracao = {
    val base = in.xmlbase.map(M.LexMLURN)
    val id = strToID(in.id.getOrElse("expected id in Alteracao: " + in))
    val mixedElements : M.Mixed[M.AlteracaoElement] = M.Mixed(in.mixed.map(d => scalaxbToEitherAlteracaoElement(d.key,d.value)))       
    M.Alteracao(
        id = id,
        base = base,
        mixedElems = mixedElements 
        )       
  }
   
  def scalaxbToEitherAlteracaoElement(label : Option[String], value : Any) : Either[M.AlteracaoElement,String] = (label,value) match {
    case (None,x : String) => Right(x)    
    case (Some(l),x) => Left(scalaxbAlteracaoElementToModel(l,x))
    case (l,x) => sys.error("Unexpected in scalaxbToEitherAlteracaoElement: " + ((l,x)))
  }
  
  def scalaxbAlteracaoElementToModel(l : String, v : Any) : M.AlteracaoElement = (l,v) match {
    case (_,x : X.GenInline) => scalaxbToModel3(l,x) match {
      case y : M.AlteracaoElement => y
      case z => sys.error("Expecting type M.AlteracaoElement, found: " + z + " (class: " + z.getClass.getName + ")")
    }
    case (_, x : X.DispositivoType) => scalaxbToModel1(l,x)
    case (_, x : X.Hierarchy) => scalaxbToModel(l,x)
    case (_, x : X.Omissis) => scalaxbToModel(x)
    case _ => sys.error("Unsupported in AlteracaoElement: (" + l + "): " + v)
    /**
     * <xsd:group name="AlteracaoElement">
        <xsd:choice>            
            GenInline (td, Ementa, sub, ins, dfn, b, NomeAgrupador, RemissaoMultipla, 
            			Remissao, Bloco, EmLinha, span, a, TituloDispositivo,
            			del, i, p,  sup, Epigrafe, th, 
            TextoSimplesOptType (Observacao, FormulaPromulgacao, LocalDataFecho)
            TextoTypable (trait)  (Preambulo)
            	Nota ( Notas, Nota, NotaReferenciada)
            	TextoType         
            Omissis
            DispositivoType (DispositivoGenerico, Paragrafo, Inciso, Alinea, Artigo, Caput, Item, Pena)
            Hierarchy (Titulo, Secao, Livro, Capitulo, Parte, Subsecao, AgrupamentoHierarquico)                  
            BlockContainer (div, Agrupamento, Texto) 
               
        </xsd:choice>
    </xsd:group>
     */    
  }
  
  def scalaxbToModel(label : Option[String], in : X.LXhierOption) : M.HierarchicalElement = {  
    (label,in) match {
      case (_,o : X.Omissis) => scalaxbToModel(o)
      case (Some(l),x : X.Hierarchy) => scalaxbToModel(l,x)
      case (Some(l),x : X.DispositivoType) => scalaxbToModel1(l,x) match {
        case y : M.Artigo => y
        case _ => sys.error("Expected Artigo at " + ((l,x)))
      }
      case _ => sys.error("Unsupported or unexpected combination in scalaxbToModel(Option[String],X.LXhierOption): " + ((label,in)))
    } 
  }
 
  type SomeHasInlineSeq = M.HasInlineSeq[T] forSome { type T <: M.HasInlineSeq[T] } 
  
  def scalaxbToModel3(label : String, in : X.GenInline) : SomeHasInlineSeq = {
    val inl = scalaxbToModel(in)
    def attr[T](name : String, f : X.GenInline => Option[T]) : T
        = f(in).getOrElse(sys.error("Missing attribute " + name + " in " + in))
        
    def optAttr[T,R](f : X.GenInline => Option[T],g : T => R) : Option[R]
        = f(in).map(g)
        
    val abreAspas = in.abreAspas == Some(X.S)
    val fechaAspas = in.fechaAspas == Some(X.S)   
    val notaAlteracao = in.notaAlteracao    
    
    label match {  
      case "Ementa" => M.Ementa(inl,abreAspas,fechaAspas,notaAlteracao) : SomeHasInlineSeq
      case "Epigrafe" => M.Epigrafe(inl,abreAspas,fechaAspas,notaAlteracao) : SomeHasInlineSeq
      case "NomeAgrupador" => M.NomeAgrupador(inl) : SomeHasInlineSeq
      case "RemissaoMultipla" => 
        M.RemissaoMultipla(
            base = M.LexMLURN(attr("xml:base",_.xmlbase)),
            inlineSeq = inl) : SomeHasInlineSeq
      case "Remissao" => M.Remissao(
            href = M.LexMLURN(attr("xlink:href",_.xlinkhref)),
            inlineSeq = inl) : SomeHasInlineSeq
      case "Bloco" => M.Bloco(
           nome = attr("nome",_.nome),
           inlineSeq = inl
          ) : SomeHasInlineSeq
      case "EmLinha" => M.EmLinha(
           nome = attr("nome",_.nome),
           inlineSeq = inl
          ) : SomeHasInlineSeq
      case "TituloDispositivo" => M.TituloDispositivo(inl)
      case "span" => M.Span(
            href = M.LexMLURN(attr("xlink:href",_.xlinkhref)),
            inlineSeq = inl
          ) : SomeHasInlineSeq
          
      case "a" => M.Anchor(
            href = M.LexMLURN(attr("xlink:href",_.xlinkhref)),
            name = in.name,
            target = in.target,
            inlineSeq = inl
          ) : SomeHasInlineSeq
      case "p" => M.Paragraph(inl,abreAspas,fechaAspas,notaAlteracao) : SomeHasInlineSeq
      case "th" => sys.error("Tables (th) are unsupported at the moment: " + in) : SomeHasInlineSeq
      case "td" => sys.error("Tables (td) are unsupported at the moment: " + in) : SomeHasInlineSeq
      case "i" => M.GenHtmlInlineElement(M.TGHIE_I,inl) : SomeHasInlineSeq
      case "b" => M.GenHtmlInlineElement(M.TGHIE_B,inl) : SomeHasInlineSeq
      case "del" => M.GenHtmlInlineElement(M.TGHIE_Del,inl) : SomeHasInlineSeq      
      case "sub" => M.GenHtmlInlineElement(M.TGHIE_Sub,inl) : SomeHasInlineSeq
      case "ins" => M.GenHtmlInlineElement(M.TGHIE_Ins,inl) : SomeHasInlineSeq
      case "dfn" => M.GenHtmlInlineElement(M.TGHIE_Dfn,inl) : SomeHasInlineSeq
      case "sup" => M.GenHtmlInlineElement(M.TGHIE_Sup,inl) : SomeHasInlineSeq

    }
  }
  def scalaxbToModel(in : X.GenInline) : M.InlineSeq = {
    val lang = if (in.xmllang == "pt-BR") { None } else { Some(M.Lang(in.xmllang)) }
    type E = Either[M.InlineElement,String]
    val elems : Seq[E] = in.mixed.map(x => (x.key,x.value)).collect {
      case (None,x : String) => Right(x) : E
      case (Some(l),x : X.GenInline) => scalaxbToModel3(l,x) match {
        case y : M.InlineElement => Left(y) : E
        case z => sys.error("Unexpected " + z + " in " + in) : E
      }     
    }    
    M.InlineSeq(mixedElems = M.Mixed(elems), lang = lang)
  }
  
}