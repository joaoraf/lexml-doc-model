package br.gov.lexml.doc

import java.net.URI
import scala.language.existentials

final case class LexmlDocument(
    metadado : Metadado,
    contents : DocumentContents)

/**
 * ID
 */
    
final case class ID(uri : URI)

sealed trait HasID extends Product {
  val id : ID
}

final case class LexMLURN(uri : URI) 

final case class IDREF()

/**
 * General Types
	*/



sealed trait HasLang extends Product {
  val lang : Option[Lang]
}

final case class Lang(code : String = Lang.DEFAULT) {
  val isDefault = code == Lang.DEFAULT
}

object Lang {
  val DEFAULT = "pt_BR"
  def simplify(f : Option[Lang]) : Option[Lang] =
    f.filterNot(_.isDefault)
}

final case class Mixed[T](elems : Seq[Either[T,String]] = Seq()) {
  val empty = elems.isEmpty
}    
    
/**
 * Metadado    
 */
    
final case class Metadado(
    identificacao : Identificacao
    )

final case class Identificacao(urn : LexMLURN)     
    
    
/**
 * Document Contents
 */


final case class InlineSeq(mixedElems : Mixed[InlineElement] = Mixed(), lang : Option[Lang] = None) extends HasLang {
  lazy val empty = mixedElems.empty
  final def normalized = InlineSeq(mixedElems,Lang.simplify(lang))
}

trait HasInlineSeq[T <: HasInlineSeq[T]] extends Product {
  val inlineSeq : InlineSeq
  final lazy val empty = inlineSeq.empty
  def mapInlineSeq(f : InlineSeq => InlineSeq) : T
  def normalized = mapInlineSeq(_.normalized)
}

object HasInlineSeq {
  def simplify[T <: HasInlineSeq[T]](x : Option[T]) : Option[T] = x match {
    case None => None
    case Some(y) => 
      val z = y.normalized
      if(z.empty) { None } else { Some(z) }
  }   
}


trait HasInlineSeqs[T <: HasInlineSeq[T],Q <: HasInlineSeqs[T,Q]] {
  val inlineSeqs : Seq[T]
  def mapInlineSeqs(f : Seq[T] => Seq[T]) : Q 
  final lazy val empty = inlineSeqs.forall(_.empty)
  final def normalized = mapInlineSeqs { x =>
    x.flatMap(y => HasInlineSeq.simplify(Some(y)))
  }
}

object HasInlineSeqs {
  def simplify[T <: HasInlineSeq[T],Q <: HasInlineSeqs[T,Q]](x : Option[Q]) : Option[Q] =
    x.map(_.normalized).filterNot(_.empty)    
}

abstract sealed class TipoLista extends Product

case object UL extends TipoLista

case object OL extends TipoLista

sealed trait LI_Item extends Product

sealed trait InlineElement extends Product with LI_Item

sealed trait LXInlineElement extends InlineElement


abstract sealed class DocumentContentsType extends Product

case object DCT_Norma extends DocumentContentsType

case object DCT_ProjetoNorma extends DocumentContentsType

case object DCT_Jurisprudencia extends DocumentContentsType

case object DCT_DocumentoGenerico extends DocumentContentsType

case object DCT_Anexo extends DocumentContentsType

abstract sealed class DocumentContents(val dcType : DocumentContentsType) extends Product

final case class Norma(contents : HierarchicalStructure) extends DocumentContents(DCT_Norma)

final case class ProjetoNorma(
    norma : Norma,
    justificacao : Seq[Justificacao] = Seq(),
    autorProjeto : Seq[String] = Seq()) extends DocumentContents(DCT_ProjetoNorma)

final case class Justificacao()

/**
 * Hierarchical Structure
 */

final case class HierarchicalStructure(
    articulacao : Articulacao,
    formulaPromulgacao : Option[FormulaPromulgacao] = None,
    epigrafe : Option[Epigrafe] = None,
    ementa : Option[Ementa] = None,
    preambulo : Option[Preambulo] = None) {
  def normalized : HierarchicalStructure = 
    HierarchicalStructure(
        articulacao = articulacao.normalized,
        formulaPromulgacao = HasInlineSeq.simplify(formulaPromulgacao),
        epigrafe = HasInlineSeq.simplify(epigrafe),
        ementa = HasInlineSeq.simplify(ementa),
        preambulo = HasInlineSeqs.simplify[PreambuloLine,Preambulo](preambulo))
        
}

final case class FormulaPromulgacao(
    inlineSeq : InlineSeq,
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeq[FormulaPromulgacao]
  with AlteracaoElement {
  
  override def mapInlineSeq(f : InlineSeq => InlineSeq) = 
    copy(inlineSeq = f (inlineSeq))
}

final case class Epigrafe(
    inlineSeq : InlineSeq = InlineSeq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeq[Epigrafe] 
  with AlteracaoElement {
  override def mapInlineSeq(f : InlineSeq => InlineSeq) = 
    copy(inlineSeq = f (inlineSeq))
}

final case class Ementa(
    inlineSeq : InlineSeq = InlineSeq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeq[Ementa] 
  with AlteracaoElement {
  override def mapInlineSeq(f : InlineSeq => InlineSeq) = 
    copy(inlineSeq = f (inlineSeq))
}

final case class PreambuloLine(inlineSeq : InlineSeq = InlineSeq()) extends HasInlineSeq[PreambuloLine] {
  override def mapInlineSeq(f : InlineSeq => InlineSeq) = 
    copy(inlineSeq = f (inlineSeq))
}

final case class Preambulo(
    inlineSeqs : Seq[PreambuloLine] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeqs[PreambuloLine,Preambulo] 
  with AlteracaoElement {
  override def mapInlineSeqs(f : Seq[PreambuloLine] => Seq[PreambuloLine]) = 
    copy(inlineSeqs = f (inlineSeqs))
}

/**
 * Articulacao
 */

sealed trait HierarchicalElement extends Product with HasID with AlteracaoElement 

/* Agrupadores:
 * 
 * Parte
 * Livro
 * Titulo
 * Capitulo
 * Secao
 * Subsecao
 * AgrupadorHierarquico
 */

abstract sealed class TipoAgrupador extends Product

abstract sealed class Agrupador extends HierarchicalElement {
  val tipoAgrupador : TipoAgrupador
  val rotulo : Option[Rotulo] 
  val nomeAgrupador : Option[NomeAgrupador]
  val id : ID
  val elems : Seq[HierarchicalElement] 
}

final case class Rotulo(rotulo : String)

final case class NomeAgrupador(inlineSeq : InlineSeq) extends HasInlineSeq[NomeAgrupador] {
  override def mapInlineSeq(f : InlineSeq => InlineSeq) = NomeAgrupador(f(inlineSeq))
}

case object TA_Generico extends TipoAgrupador

abstract sealed class TipoAgrupadorPredef extends TipoAgrupador 

case object TAP_Parte extends TipoAgrupadorPredef 

case object TAP_Livro extends TipoAgrupadorPredef

case object TAP_Titulo extends TipoAgrupadorPredef

case object TAP_Capitulo extends TipoAgrupadorPredef

case object TAP_Secao extends TipoAgrupadorPredef

case object TAP_Subsecao extends TipoAgrupadorPredef

final case class AgrupadorPredef(
    tipoAgrupador : TipoAgrupadorPredef,
    id : ID,    
    rotulo : Option[Rotulo] = None,
    nomeAgrupador : Option[NomeAgrupador] = None,
    elems : Seq[HierarchicalElement] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends Agrupador

final case class AgrupadorGenerico(
    nome : String,    
    id : ID,    
    rotulo : Option[Rotulo] = None,
    nomeAgrupador : Option[NomeAgrupador] = None,
    elems : Seq[HierarchicalElement] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends Agrupador {
  final val tipoAgrupador = TA_Generico
}

final case class Articulacao(elems : Seq[HierarchicalElement] = Seq()) {
  def normalized = this
}

/**
 * LexML Containers
 */

abstract sealed trait LXContainer extends HasID with AlteracaoElement  

/**
 * Dispositivos
 */

abstract sealed trait TipoDispositivo extends Product

abstract sealed trait TipoDispositivoPredef extends TipoDispositivo

case object TDP_Artigo extends TipoDispositivoPredef

abstract sealed trait TipoDispositivoNaoArtigo extends TipoDispositivo

case object TDP_Caput extends TipoDispositivoNaoArtigo with TipoDispositivoPredef

case object TDP_Paragrafo extends TipoDispositivoNaoArtigo with TipoDispositivoPredef

case object TDP_Inciso extends TipoDispositivoNaoArtigo with TipoDispositivoPredef

case object TDP_Alinea extends TipoDispositivoNaoArtigo with TipoDispositivoPredef

case object TDP_Item extends TipoDispositivoNaoArtigo with TipoDispositivoPredef

case object TDP_Pena extends TipoDispositivoNaoArtigo with TipoDispositivoPredef

case object TD_Generico extends TipoDispositivoNaoArtigo

trait Dispositivo extends LXContainer {
  val tipoDispositivo : TipoDispositivo
  val titulo : Option[TituloDispositivo]
  val rotulo : Option[Rotulo] 
  val conteudo : Option[ConteudoDispositivo]  
  val alteracao : Option[Alteracao]
  val containers : Seq[LXContainer]    
}

final case class ParagrafoTextoDispositivo(inlineSeq : InlineSeq) 
  extends HasInlineSeq[ParagrafoTextoDispositivo] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) = ParagrafoTextoDispositivo(f(inlineSeq))
}

abstract sealed trait ConteudoDispositivo extends Product

final case class TextoDispositivo(inlineSeqs : Seq[ParagrafoTextoDispositivo]) 
    extends HasInlineSeqs[ParagrafoTextoDispositivo,TextoDispositivo] 
      with ConteudoDispositivo {
  def mapInlineSeqs(f : Seq[ParagrafoTextoDispositivo] => Seq[ParagrafoTextoDispositivo]) =
    TextoDispositivo(f(inlineSeqs))
}

case object OmissisSimples extends ConteudoDispositivo

final case class TituloDispositivo(inlineSeq : InlineSeq) extends HasInlineSeq[TituloDispositivo] {
  override def mapInlineSeq(f : InlineSeq => InlineSeq) = 
    TituloDispositivo(f(inlineSeq))
}

trait DispositivoPredef extends Dispositivo {
  val tipoDispositivoPredef : TipoDispositivoPredef 
  override val tipoDispositivo : TipoDispositivo = tipoDispositivoPredef
}

trait DispositivoNaoArtigo extends Dispositivo {
  val tipoDispositivoNaoArtigo : TipoDispositivoNaoArtigo
  override val tipoDispositivo : TipoDispositivo = tipoDispositivoNaoArtigo
}

final case class DispositivoPredefNA(
    id : ID,
    override val tipoDispositivo : TipoDispositivoPredef with TipoDispositivoNaoArtigo,
    titulo : Option[TituloDispositivo] = None,
    rotulo : Option[Rotulo] = None,
    conteudo : Option[ConteudoDispositivo] = None,
    alteracao : Option[Alteracao] = None,
    containers : Seq[LXContainer] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]        
)  extends DispositivoPredef with DispositivoNaoArtigo {    
  val tipoDispositivoPredef = tipoDispositivo
  val tipoDispositivoNaoArtigo = tipoDispositivo  
  Ensuring(this).ensuring(tipoDispositivo != null, "tipoDispositivo is null!")
}

final case class DispositivoGenerico(
    id : ID,
    titulo : Option[TituloDispositivo] = None,
    rotulo : Option[Rotulo] = None,
    conteudo : Option[ConteudoDispositivo] = None,
    alteracao : Option[Alteracao] = None,
    containers : Seq[LXContainer] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends DispositivoNaoArtigo {
  val tipoDispositivoNaoArtigo = TD_Generico
}
    

/**
 * Artigo
 */

final case class Artigo(
    id : ID,
    titulo : Option[TituloDispositivo] = None,
    rotulo : Option[Rotulo] = None,
    conteudo : Option[ConteudoDispositivo] = None,
    alteracao : Option[Alteracao] = None,
    containers : Seq[LXContainer] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends DispositivoPredef with HierarchicalElement {
  val tipoDispositivoPredef = TDP_Artigo
  override val tipoDispositivo = TDP_Artigo   
}

/**
 *  Omissis
 */

final case class Omissis(
    id : ID, abreAspas : Boolean = false, fechaAspas : Boolean = false,
    notaAlteracao : Option[String] = None) extends HierarchicalElement with LXContainer

/**
 * Alteracao
 */
    
final case class Alteracao(
    id : ID,
    base : Option[LexMLURN] = None,
    mixedElems : Mixed[AlteracaoElement] = Mixed()) extends HasID 

abstract sealed trait AlteracaoElement extends Product {
  val abreAspas : Boolean
  val fechaAspas : Boolean
  val notaAlteracao : Option[String]
}

/**
 * <xsd:element ref="FormulaPromulgacao"/>
            <xsd:element ref="Epigrafe"/>
            <xsd:element ref="Ementa"/>
            <xsd:element ref="Preambulo"/>
 */



/**
 * Inline elements    
 */




final case class Remissao(href : LexMLURN, inlineSeq : InlineSeq = InlineSeq()) extends LXInlineElement with HasInlineSeq[Remissao] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) = Remissao(href,f(inlineSeq))
}

final case class RemissaoMultipla(base : LexMLURN,inlineSeq : InlineSeq = InlineSeq()) extends LXInlineElement with HasInlineSeq[RemissaoMultipla] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) = RemissaoMultipla(base,f(inlineSeq))
}

final case class Formula() extends LXInlineElement

final case class EmLinha(nome : String, inlineSeq : InlineSeq = InlineSeq()) extends InlineElement with HasInlineSeq[EmLinha] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) =
    EmLinha(nome,f(inlineSeq))
}

/**
 * Blocos
 */

abstract sealed trait BlockElement extends AlteracaoElement 

final case class ConteudoExterno(
    conteudo : scala.xml.NodeSeq = scala.xml.NodeSeq.Empty) extends BlockElement {
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None
}

final case class Bloco(
    nome : String,
    inlineSeq : InlineSeq = InlineSeq()) extends BlockElement with HasInlineSeq[Bloco] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) = Bloco(nome,f(inlineSeq))
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None
}

/**
 * Blocos HTML
 */

abstract sealed trait HTMLBlock extends BlockElement

final case class Paragraph(
    inlineSeq : InlineSeq, 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeq[Paragraph] with HTMLBlock with LI_Item {
  def mapInlineSeq(f : InlineSeq => InlineSeq) =
      copy(inlineSeq = f(inlineSeq))
      
}

final case class HTMLList(
    tipoLista : TipoLista, 
    itens : Seq[LI] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HTMLBlock with LI_Item



final case class LI(elems : Mixed[LI_Item] = Mixed()) 



final case class Table() extends HTMLBlock {
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None
}

/**
 * Containers
 */

abstract sealed trait Container extends AlteracaoElement with HasID {
  val id : ID
  val elems : Seq[BlockElement]
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None
}

final case class Div(id : ID, elems : Seq[BlockElement] = Seq()) extends Container

final case class Agrupamento(
    nome : String,
    id : ID,
    elems : Seq[BlockElement] = Seq()) extends Container 

    
/**
 * HTML inline elements    
 */

abstract sealed class TipoHtmlInlineElement extends Product    

case object THIE_Anchor extends TipoHtmlInlineElement

case object THIE_Span extends TipoHtmlInlineElement

sealed trait HTMLinlineElement extends InlineElement  {
  val tipoHtmlInlineElement : TipoHtmlInlineElement
}

final case class Anchor(
    href : LexMLURN,
    name : Option[String] = None,
    target : Option[String] = None,
    inlineSeq : InlineSeq = InlineSeq()) extends HTMLinlineElement with HasInlineSeq[Anchor] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) = copy(inlineSeq = f(inlineSeq))
  val tipoHtmlInlineElement = THIE_Anchor
}

final case class Span(href : LexMLURN,
    inlineSeq : InlineSeq = InlineSeq()) extends HTMLinlineElement with HasInlineSeq[Span] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) = copy(inlineSeq = f(inlineSeq))
  val tipoHtmlInlineElement = THIE_Span
}

abstract sealed class TipoGenHtmlInlineElement extends TipoHtmlInlineElement 

case object TGHIE_B extends TipoGenHtmlInlineElement
case object TGHIE_I extends TipoGenHtmlInlineElement
case object TGHIE_Sub extends TipoGenHtmlInlineElement
case object TGHIE_Sup extends TipoGenHtmlInlineElement
case object TGHIE_Ins extends TipoGenHtmlInlineElement
case object TGHIE_Del extends TipoGenHtmlInlineElement
case object TGHIE_Dfn extends TipoGenHtmlInlineElement

final case class GenHtmlInlineElement(
    tipoHtmlInlineElement : TipoGenHtmlInlineElement,
    inlineSeq : InlineSeq = InlineSeq()) extends HTMLinlineElement with HasInlineSeq[GenHtmlInlineElement] {
  def mapInlineSeq(f : InlineSeq => InlineSeq) = copy(inlineSeq = f(inlineSeq))
}

/**
 * LexML Markers
 */

final case class NotaReferenciada(nota : IDREF) extends InlineElement

final case class Marcador() extends InlineElement

final case class Img() extends InlineElement

