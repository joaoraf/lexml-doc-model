package br.gov.lexml.doc.xml

import br.gov.lexml.schema.scala.data as X
import br.gov.lexml.doc as M

import java.net.URI
import br.gov.lexml.doc.{Ementa, HasInlineSeq, LexmlDocument, Norma, ProjetoNorma, RemissaoMultipla, TituloDispositivo}
import br.gov.lexml.schema.scala.data.{Assinatura, AssinaturaGrupo, ParsType}

import scala.language.existentials
import org.slf4j.LoggerFactory

import scala.annotation.unused

object XmlConverter:
  private val logger = LoggerFactory.getLogger(this.getClass)
  
  def scalaxbToModel(md : X.Metadado) : M.Metadado =
    val xNotas = md.Notas.flatMap(_.notassequence1.map(_.Nota))
    val notas = xNotas.map { n =>
      val id = n.id
      val autor = n.autor
      val pl = n.p.map(scalaxbToModel).map(x => M.Paragraph(x))
      M.Nota(id = id, autor = autor, contents = pl)
    }
    M.Metadado(identificacao = M.Identificacao(M.LexMLURN(md.Identificacao.URN)), notas = notas)
  
  trait SomeLexmlDocument:
    type T <: M.DocumentContents[T]
    val value : M.LexmlDocument[T]
  object SomeLexmlDocument:
    def apply[Q <: M.DocumentContents[Q]](v: M.LexmlDocument[Q]): SomeLexmlDocument { type T = Q } =
      new SomeLexmlDocument:
        override type T = Q
        override val value: M.LexmlDocument[Q] = v

  def scalaxbToModel[T <: M.DocumentContents[T]](dt : X.LexML) : SomeLexmlDocument  =
    val md = scalaxbToModel(dt.Metadado)    
    val co = dt.documentContentOption2
    (co.key,co.value) match {
      case (_,pj : X.ProjetoNorma) =>        
        SomeLexmlDocument(M.LexmlDocument(md,scalaxbToModel(pj)))
      case (Some("Norma"),norma : X.HierarchicalStructure) =>
        SomeLexmlDocument(M.LexmlDocument(md, M.Norma(scalaxbToModel(norma))))
      case (key,value) =>
        sys.error(s"Tipo de documento não suportado. Label = $key, conteudo = $value")
    }

  def scalaxbToModel(pj : X.ProjetoNorma) : M.ProjetoNorma =
    M.ProjetoNorma(M.Norma(scalaxbToModel(pj.Norma)))
       
  def scalaxbToModel(in : X.TextoTypable) : Seq[M.InlineSeq] = in.p.map(scalaxbToModel)      
  
  def scalaxbToModel(hs : X.HierarchicalStructure) : M.HierarchicalStructure =
    val articulacao = M.Articulacao(hs.Articulacao.lXhierOption1.map(x => scalaxbToModel(x.key,x.value) : M.HierarchicalElement))
    val res1 = M.HierarchicalStructure(articulacao = articulacao)
    val res2 = hs.ParteInicial.map { pi =>
      val ementa = pi.Ementa.map(x => 
        M.Ementa(
            scalaxbToModel(x),
            abreAspas = x.abreAspas.contains(X.S),
            fechaAspas = x.fechaAspas.contains(X.S),
            notaAlteracao = x.notaAlteracao))
      val formulaPromulgacao = pi.FormulaPromulgacao.map(x =>
            M.FormulaPromulgacao(
                x.p.map(scalaxbToModel).map(y => M.Paragraph(inlineSeq = y))
                ) )
          
      val epigrafe = pi.Epigrafe.map(x => M.Epigrafe(
          scalaxbToModel(x),
          abreAspas = x.abreAspas.contains(X.S),
          fechaAspas = x.fechaAspas.contains(X.S),
          notaAlteracao = x.notaAlteracao))
      val preambulo = pi.Preambulo.map(x => 
        M.Preambulo(
            scalaxbToModel(x).map(M.PreambuloLine.apply),
            abreAspas = x.abreAspas.contains(X.S),
            fechaAspas = x.fechaAspas.contains(X.S),
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
      val pfAssinaturas : Seq[M.ParteFinalAssinatura] = pf.partefinaloption.map { x => parseParteFinalOption(x.value) }
      res2.copy(localDataFecho = ldf,assinaturas = pfAssinaturas)
    }.getOrElse(res2)
    res3
  end scalaxbToModel

  def parseParteFinalOption(pfo: X.ParteFinalOption) : M.ParteFinalAssinatura = pfo match {
    case x : X.AssinaturaGrupoOption => parseAssinaturaGrupoOption(x)
    case AssinaturaGrupo(nomeGrupo, assinaturas) => M.AssinaturaGrupo(nomeGrupo = nomeGrupo,
      assinaturas = assinaturas.map { x => parseAssinaturaGrupoOption(x.value) })
  }

  def parseAssinaturaGrupoOption(ago : X.AssinaturaGrupoOption) : M.ElementoAssinaturaGrupo = ago match {
    case Assinatura(nomePessoa, cargo) => M.Assinatura(nomes = nomePessoa, cargos = cargo)
    case ParsType(p) => M.AssinaturaTexto(p.map(scalaxbToModel).map(y => M.Paragraph(inlineSeq = y)))
  }

  def scalaxbToModel(in : X.Omissis) : M.Omissis = 
    M.Omissis(
        id = strToID(in.id),
        abreAspas = in.abreAspas == Some(X.S),
        fechaAspas = in.fechaAspas == Some(X.S),
        notaAlteracao = in.notaAlteracao
        )
  
  val tiposAgrupador = Map[String,M.TipoAgrupador](
    "Parte" -> M.TAP.Parte,
    "Livro" -> M.TAP.Livro,
    "Titulo" -> M.TAP.Titulo,
    "Subtitulo" -> M.TAP.Subtitulo,
    "Capitulo" -> M.TAP.Capitulo,
    "Secao" -> M.TAP.Secao,
    "Subsecao" -> M.TAP.Subsecao,
    "AgrupamentoHierarquico" -> M.TA_Generico
      )
      
  def strToID(in : String) : M.ID = M.ID(URI.create(in))
  
  @unused
  def strToLexMLURN(in : String) : M.LexMLURN = M.LexMLURN(URI.create(in))
  
  def scalaxbToModel(label : String, in : X.Hierarchy) : M.HierarchicalElement =
    val tipo = tiposAgrupador.getOrElse(label,sys.error("Unexpected label for tiposAgrupador: " + label))
    val id = strToID(in.id.getOrElse(sys.error("Expecting id in " + ((label,in)))))
    val rotulo = in.Rotulo.map(M.Rotulo.apply)
    val nomeAgrupador = in.NomeAgrupador.map(x => M.NomeAgrupador(scalaxbToModel(x)))
    val elems = in.lXhierOption3.map(x => scalaxbToModel(x.key,x.value))
    val abreAspas = in.abreAspas.contains(X.S)
    val fechaAspas = in.fechaAspas.contains(X.S)
    val notaAlteracao = in.notaAlteracao
    tipo match {
      case t : M.TAP =>
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
            nome = in.nome.getOrElse(sys.error(s"Missing attribute 'nome' in ($label) $in")),
            id = id,
            rotulo = rotulo,
            nomeAgrupador = nomeAgrupador,
            elems = elems, 
            abreAspas = abreAspas, 
            fechaAspas = fechaAspas,            
            notaAlteracao = notaAlteracao
            )
    }
  
  val tiposDispositivo = Map[String,M.TipoDispositivo](
    "Artigo" -> M.TDP.Artigo,
    "Caput" -> M.TDP.Caput,
    "Paragrafo" -> M.TDP.Paragrafo,
    "Inciso" -> M.TDP.Inciso,
    "Alinea" -> M.TDP.Alinea,
    "Item" -> M.TDP.Item,
    "Pena" -> M.TDP.Pena,
    "DispositivoGenerico" -> M.TD_Generico )
  
  def scalaxbToModel1(label : String, in : X.DispositivoType) : M.Dispositivo =
    val tipo = tiposDispositivo.getOrElse(label, sys.error("Unexpected label in tiposDispositivo: " + ((label,in)))) ;                   
    val id = strToID(in.id.getOrElse(sys.error("Expecting id in " + ((label,in)))))
    val rotulo = in.Rotulo.map(M.Rotulo.apply)
    val titulo = in.TituloDispositivo.map(x => M.TituloDispositivo(scalaxbToModel(x)))
    val abreAspas = in.abreAspas.contains(X.S)
    val fechaAspas = in.fechaAspas.contains(X.S)
    val notaAlteracao = in.notaAlteracao
    val conteudo : Option[M.ConteudoDispositivo] =
      if in.textoOmitido.contains(X.S) then
        Some(M.OmissisSimples) 
      else if in.p.nonEmpty then
        Some(M.TextoDispositivo(in.p.map(x => M.ParagrafoTextoDispositivo(scalaxbToModel(x))))) 
      else
        None
    val alteracao : Option[M.Alteracao] = in.Alteracao.map(scalaxbToModel)
    val nome = in.nome
    val containers : Seq[M.LXContainer] = in.lXcontainersOmissisOption5.map(d => (d.key,d.value)).collect {
      case (_,x : X.Omissis) => scalaxbToModel(x) : M.LXContainer 
      case (Some(l),x : X.DispositivoType) => scalaxbToModel1(l,x) : M.Dispositivo
      case (None,x) => sys.error("Expected label at " + x)
    } ++ in.Pena.map(x => scalaxbToModel1("Pena",x) : M.Dispositivo)    
    
    tipo match {
      case t :  M.TipoDispositivoNaoArtigo with M.TDP =>
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
          nome = nome.getOrElse(throw new RuntimeException("DispositivoGenerico sem nome: " + in)),
          titulo = titulo, 
          rotulo = rotulo, 
          conteudo = conteudo, 
          alteracao = alteracao, 
          containers = containers, abreAspas, fechaAspas, notaAlteracao)
      case M.TDP.Artigo =>
        M.Artigo(
          id = id,           
          titulo = titulo, 
          rotulo = rotulo, 
          conteudo = conteudo, 
          alteracao = alteracao, 
          containers = containers, abreAspas, fechaAspas, notaAlteracao)      
    }
  end scalaxbToModel1

  def scalaxbToModel(in : X.Alteracao) : M.Alteracao =
    val base = in.xmlbase.map(M.LexMLURN.apply)
    val id = strToID(in.id.getOrElse(s"expected id in Alteracao: $in"))
    val mixedElements : M.Mixed[M.AlteracaoElement] = M.Mixed(in.mixed.map(d => scalaxbToEitherAlteracaoElement(d.key,d.value)))       
    M.Alteracao(
        id = id,
        base = base,
        mixedElems = mixedElements 
        )
   
  def scalaxbToEitherAlteracaoElement(label : Option[String], value : Any) : Either[M.AlteracaoElement,String] = (label,value) match {
    case (None,x : String) => Right(x)    
    case (Some(l),x) => Left(scalaxbAlteracaoElementToModel(l,x))
    case (l,x) => sys.error(s"Unexpected in scalaxbToEitherAlteracaoElement: ${(l,x)}")
  }
  
  def scalaxbAlteracaoElementToModel(l : String, v : Any) : M.AlteracaoElement = (l,v) match {
    case (_,x : X.GenInline) => scalaxbToModel3(l,x).value match {
      case y : M.AlteracaoElement => y
      case z => sys.error(s"Expecting type M.AlteracaoElement, found: $z (class: ${z.getClass.getName})")
    }
    case (_, x : X.DispositivoType) => scalaxbToModel1(l,x)
    case (_, x : X.Hierarchy) => scalaxbToModel(l,x)
    case (_, x : X.Omissis) => scalaxbToModel(x)
    case _ => sys.error(s"Unsupported in AlteracaoElement: ($l): $v")
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
  
  def scalaxbToModel(label : Option[String], in : X.LXhierOption) : M.HierarchicalElement =
    (label,in) match {
      case (_,o : X.Omissis) => scalaxbToModel(o)
      case (Some(l),x : X.Hierarchy) => scalaxbToModel(l,x)
      case (Some(l),x : X.DispositivoType) => scalaxbToModel1(l,x) match {
        case y : M.Artigo => y
        case _ => sys.error(s"Expected Artigo at ${(l,x)}")
      }
      case _ => sys.error(s"Unsupported or unexpected combination in scalaxbToModel(Option[String],X.LXhierOption): ${(label,in)}")
    }
 
  trait SomeHasInlineSeq:
    type T <: M.HasInlineSeq[T]
    val value : M.HasInlineSeq[T]

  object SomeHasInlineSeq:
    def apply[Q <: M.HasInlineSeq[Q]](v : HasInlineSeq[Q]) : SomeHasInlineSeq = new SomeHasInlineSeq:
      override type T = Q
      override val value: HasInlineSeq[T] = v

  def scalaxbToModel3(label : String, in : X.GenInline) : SomeHasInlineSeq =
    val inl = scalaxbToModel(in)
    def attr[T](name : String, f : X.GenInline => Option[T]) : T
        = f(in).getOrElse(sys.error("Missing attribute " + name + " in " + in))
        
    def optAttr[T,R](f : X.GenInline => Option[T],g : T => R) : Option[R]
        = f(in).map(g)
        
    val abreAspas = in.abreAspas.contains(X.S)
    val fechaAspas = in.fechaAspas.contains(X.S)
    val notaAlteracao = in.notaAlteracao    
    
    label match {
      case "Ementa" => SomeHasInlineSeq(M.Ementa(inl, abreAspas, fechaAspas, notaAlteracao))
      case "Epigrafe" => SomeHasInlineSeq(M.Epigrafe(inl, abreAspas, fechaAspas, notaAlteracao))
      case "NomeAgrupador" => SomeHasInlineSeq(M.NomeAgrupador(inl))
      case "RemissaoMultipla" =>
        SomeHasInlineSeq (
          M.RemissaoMultipla(
            base = M.LexMLURN(attr("xml:base", _.xmlbase)),
            inlineSeq = inl)
        )
      case "Remissao" =>
        SomeHasInlineSeq (
          M.Remissao(
            href = M.LexMLURN(attr("xlink:href",_.xlinkhref)),
            inlineSeq = inl)
        )
      case "Bloco" =>
        SomeHasInlineSeq (
          M.Bloco(
           nome = attr("nome",_.nome),
           inlineSeq = inl
          )
        )
      case "EmLinha" =>
        SomeHasInlineSeq (
          M.EmLinha(
           nome = attr("nome",_.nome),
           inlineSeq = inl
          )
        )
      case "TituloDispositivo" => SomeHasInlineSeq(M.TituloDispositivo(inl))
      case "span" =>
        SomeHasInlineSeq (
          M.Span(
            href = M.LexMLURN(attr("xlink:href",_.xlinkhref)),
            inlineSeq = inl
          )
        )
      case "a" =>
        SomeHasInlineSeq (
          M.Anchor(
            href = M.LexMLURN(attr("xlink:href",_.xlinkhref)),
            name = in.name,
            target = in.target,
            inlineSeq = inl
          )
        )
      case "p" => SomeHasInlineSeq(M.Paragraph(inl,abreAspas,fechaAspas,notaAlteracao))
      case "th" => SomeHasInlineSeq(sys.error("Tables (th) are unsupported at the moment: " + in))
      case "td" => sys.error("Tables (td) are unsupported at the moment: " + in) : SomeHasInlineSeq
      case "i" => SomeHasInlineSeq(M.GenHtmlInlineElement(M.TGHIE.I,inl))
      case "b" => SomeHasInlineSeq(M.GenHtmlInlineElement(M.TGHIE.B,inl))
      case "del" => SomeHasInlineSeq(M.GenHtmlInlineElement(M.TGHIE.Del,inl))
      case "sub" => SomeHasInlineSeq(M.GenHtmlInlineElement(M.TGHIE.Sub,inl))
      case "ins" => SomeHasInlineSeq(M.GenHtmlInlineElement(M.TGHIE.Ins,inl))
      case "dfn" => SomeHasInlineSeq(M.GenHtmlInlineElement(M.TGHIE.Dfn,inl))
      case "sup" => SomeHasInlineSeq(M.GenHtmlInlineElement(M.TGHIE.Sup,inl))
    }
  def scalaxbToModel(in : X.GenInline) : M.InlineSeq =
    val lang = if in.xmllang == "pt-BR" then None else Some(M.Lang(in.xmllang))
    type E = Either[M.InlineElement,String]
    val elems : Seq[E] = in.mixed.map(x => (x.key,x.value)).collect {
      case (None,x : String) => Right(x) : E
      case (Some(l),x : X.GenInline) =>
        
        scalaxbToModel3(l,x).value match {
          case y : M.InlineElement => Left(y) : E
          case z => sys.error(s"Unexpected $z in $in") : E
        }
      case (Some(l),nr : X.NotaReferenciada) =>
        Left(M.NotaReferenciada(M.IDREF(nr.nota)))
      case (Some(l),x) =>
        sys.error(s"Unexpected class: l=$l, class=${x.getClass.getName}, x=$x")
    }    
    M.InlineSeq(mixedElems = M.Mixed(elems), lang = lang)
end XmlConverter