package br.gov.lexml.doc

import java.net.URI
import scala.annotation.unused
import scala.language.existentials

final case class LexmlDocument[T <: DocumentContents[T]](
    metadado : Metadado,
    contents : DocumentContents[T])

/**
 * ID
 */
    
final case class ID(uri : URI)

sealed trait HasID extends Product {
  val id : ID
}

final case class LexMLURN(uri : URI) 

final case class IDREF(id : String)

/**
 * General Types
	*/



sealed trait HasLang extends Product:
  val lang : Option[Lang]

final case class Lang(code : String = Lang.DEFAULT):
  val isDefault: Boolean = code == Lang.DEFAULT

object Lang:
  val DEFAULT = "pt_BR"
  def simplify(f : Option[Lang]) : Option[Lang] =
    f.filterNot(_.isDefault)


final case class Mixed[T](elems : Seq[Either[T,String]] = Seq()):
  val empty: Boolean = elems.isEmpty

    
/**
 * Metadado    
 */
    
final case class Metadado(
    identificacao : Identificacao,
    notas : Seq[Nota] = Seq()
    )

final case class Identificacao(urn : LexMLURN)     

final case class Nota(id : Option[String] = None, autor : Option[String] = None, contents : Seq[Paragraph] = Seq())
    
/**
 * Document Contents
 */

final case class InlineSeq(mixedElems : Mixed[InlineElement] = Mixed(), lang : Option[Lang] = None) extends HasLang:
  lazy val empty: Boolean = mixedElems.empty
  def normalized: InlineSeq = InlineSeq(mixedElems,Lang.simplify(lang))

trait HasInlineSeq[T <: HasInlineSeq[T]] extends Product:
  val inlineSeq : InlineSeq
  final lazy val empty = inlineSeq.empty
  def mapInlineSeq(f : InlineSeq => InlineSeq) : T
  def normalized: T = mapInlineSeq(_.normalized)

object HasInlineSeq:
  @unused
  def simplify[T <: HasInlineSeq[T]](x : Option[T]) : Option[T] = x match {
    case None => None
    case Some(y) => 
      val z = y.normalized
      if(z.empty) { None } else { Some(z) }
  }   

trait HasInlineSeqs[T <: HasInlineSeq[T],Q <: HasInlineSeqs[T,Q]]:
  val inlineSeqs : Seq[T]
  def mapInlineSeqs(f : Seq[T] => Seq[T]) : Q 
  final lazy val empty: Boolean = inlineSeqs.forall(_.empty)
  final def normalized: Q = mapInlineSeqs { x =>
    x.flatMap(y => HasInlineSeq.simplify(Some(y)))
  }

object HasInlineSeqs:
  def simplify[T <: HasInlineSeq[T],Q <: HasInlineSeqs[T,Q]](x : Option[Q]) : Option[Q] =
    x.map(_.normalized).filterNot(_.empty)

abstract sealed class TipoLista extends Product

@unused
case object UL extends TipoLista

@unused
case object OL extends TipoLista

sealed trait LI_Item extends Product

sealed trait InlineElement extends Product with LI_Item

sealed trait LXInlineElement extends InlineElement


abstract sealed class DocumentContentsType extends Product

case object DCT_Norma extends DocumentContentsType

case object DCT_ProjetoNorma extends DocumentContentsType

@unused
case object DCT_Jurisprudencia extends DocumentContentsType

@unused
case object DCT_DocumentoGenerico extends DocumentContentsType

@unused
case object DCT_Anexo extends DocumentContentsType

abstract sealed class DocumentContents[T <: DocumentContents[T]](@unused val dcType : DocumentContentsType) extends Product:
  def mapNorma(f : Norma => Norma) : T

final case class Norma(contents : HierarchicalStructure) extends DocumentContents[Norma](DCT_Norma):
  override def mapNorma(f : Norma => Norma): Norma = f(this)

final case class ProjetoNorma(
    norma : Norma,
    justificacao : Seq[Justificacao] = Seq(),
    autorProjeto : Seq[String] = Seq()) extends DocumentContents[ProjetoNorma](DCT_ProjetoNorma):
  override def mapNorma(f : Norma => Norma): ProjetoNorma = copy(norma = f(norma))

final case class Justificacao()

/**
 * Hierarchical Structure
 */

final case class HierarchicalStructure(
    articulacao : Articulacao,
    formulaPromulgacao : Option[FormulaPromulgacao] = None,
    epigrafe : Option[Epigrafe] = None,
    ementa : Option[Ementa] = None,
    preambulo : Option[Preambulo] = None,
    localDataFecho : Option[LocalDataFecho] = None,
    assinaturas : Seq[ParteFinalAssinatura] = Seq()):
  @unused
  def normalized : HierarchicalStructure =
    HierarchicalStructure(
        articulacao = articulacao.normalized,
        formulaPromulgacao = HasInlineSeqs.simplify[Paragraph,FormulaPromulgacao](formulaPromulgacao),
        epigrafe = HasInlineSeq.simplify(epigrafe),
        ementa = HasInlineSeq.simplify(ementa),
        preambulo = HasInlineSeqs.simplify[PreambuloLine,Preambulo](preambulo),
        localDataFecho = HasInlineSeqs.simplify[Paragraph,LocalDataFecho](localDataFecho),
        assinaturas = assinaturas)

final case class FormulaPromulgacao(
    inlineSeqs : Seq[Paragraph] = Seq()
    ) extends HasInlineSeqs[Paragraph,FormulaPromulgacao]
  with AlteracaoElement:
  override def mapInlineSeqs(f : Seq[Paragraph] => Seq[Paragraph]): FormulaPromulgacao =
    copy(inlineSeqs = f (inlineSeqs))

  val abreAspas = false
  val fechaAspas = false
  val notaAlteracao : Option[String] = None

final case class Epigrafe(
    inlineSeq : InlineSeq = InlineSeq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeq[Epigrafe] 
  with AlteracaoElement:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): Epigrafe =
    copy(inlineSeq = f (inlineSeq))

final case class Ementa(
    inlineSeq : InlineSeq = InlineSeq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeq[Ementa] 
  with AlteracaoElement:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): Ementa =
    copy(inlineSeq = f (inlineSeq))

final case class PreambuloLine(inlineSeq : InlineSeq = InlineSeq()) extends HasInlineSeq[PreambuloLine]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): PreambuloLine =
    copy(inlineSeq = f (inlineSeq))

final case class Preambulo(
    inlineSeqs : Seq[PreambuloLine] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HasInlineSeqs[PreambuloLine,Preambulo] 
  with AlteracaoElement:
  override def mapInlineSeqs(f : Seq[PreambuloLine] => Seq[PreambuloLine]): Preambulo =
    copy(inlineSeqs = f (inlineSeqs))

final case class LocalDataFecho(inlineSeqs : Seq[Paragraph] = Seq()) extends HasInlineSeqs[Paragraph,LocalDataFecho]:
  override def mapInlineSeqs(f : Seq[Paragraph] => Seq[Paragraph]): LocalDataFecho =
    copy(inlineSeqs = f (inlineSeqs))

sealed trait ParteFinalAssinatura extends Product

sealed trait ElementoAssinaturaGrupo extends ParteFinalAssinatura

final case class AssinaturaTexto(inlineSeqs : Seq[Paragraph] = Seq()) extends ElementoAssinaturaGrupo with HasInlineSeqs[Paragraph,AssinaturaTexto]:
  override def mapInlineSeqs(f : Seq[Paragraph] => Seq[Paragraph]): AssinaturaTexto =
    copy(inlineSeqs = f (inlineSeqs))

final case class Assinatura(nomes : Seq[String], cargos : Seq[String] = Seq()) extends ElementoAssinaturaGrupo

final case class AssinaturaGrupo(nomeGrupo : String, assinaturas : Seq[ElementoAssinaturaGrupo]) extends ParteFinalAssinatura

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

abstract sealed class Agrupador extends HierarchicalElement:
  val tipoAgrupador : TipoAgrupador
  val rotulo : Option[Rotulo] 
  val nomeAgrupador : Option[NomeAgrupador]
  val id : ID
  val elems : Seq[HierarchicalElement]

final case class Rotulo(rotulo : String)

final case class NomeAgrupador(inlineSeq : InlineSeq) extends HasInlineSeq[NomeAgrupador]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): NomeAgrupador = NomeAgrupador(f(inlineSeq))

case object TA_Generico extends TipoAgrupador

//Era: TipoAgrupadorPredef
enum TAP extends TipoAgrupador:
  case Parte, Livro, Titulo, Subtitulo, Capitulo, Secao, Subsecao

final case class AgrupadorPredef(
    tipoAgrupador : TAP,
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
    notaAlteracao : Option[String]) extends Agrupador:
  final val tipoAgrupador = TA_Generico

final case class Articulacao(elems : Seq[HierarchicalElement] = Seq()):
  def normalized: Articulacao = this


/**
 * LexML Containers
 */

sealed trait LXContainer extends HasID with AlteracaoElement

/**
 * Dispositivos
 */

sealed trait TipoDispositivo extends Product

sealed trait TipoDispositivoNaoArtigo extends TipoDispositivo

enum TDP extends TipoDispositivo:
  case Artigo
  case Caput extends TDP with TipoDispositivoNaoArtigo
  case Paragrafo extends TDP with TipoDispositivoNaoArtigo
  case Inciso extends TDP with TipoDispositivoNaoArtigo
  case Alinea extends TDP with TipoDispositivoNaoArtigo
  case Item extends TDP with TipoDispositivoNaoArtigo
  case Pena extends TDP with TipoDispositivoNaoArtigo

case object TD_Generico extends TipoDispositivoNaoArtigo

trait Dispositivo extends LXContainer:
  val tipoDispositivo : TipoDispositivo
  val titulo : Option[TituloDispositivo]
  val rotulo : Option[Rotulo] 
  val conteudo : Option[ConteudoDispositivo]  
  val alteracao : Option[Alteracao]
  val containers : Seq[LXContainer]

final case class ParagrafoTextoDispositivo(inlineSeq : InlineSeq) 
  extends HasInlineSeq[ParagrafoTextoDispositivo]:
  def mapInlineSeq(f : InlineSeq => InlineSeq): ParagrafoTextoDispositivo = ParagrafoTextoDispositivo(f(inlineSeq))

sealed trait ConteudoDispositivo extends Product

final case class TextoDispositivo(inlineSeqs : Seq[ParagrafoTextoDispositivo]) 
    extends HasInlineSeqs[ParagrafoTextoDispositivo,TextoDispositivo] 
      with ConteudoDispositivo:
  override def mapInlineSeqs(f : Seq[ParagrafoTextoDispositivo] => Seq[ParagrafoTextoDispositivo]): TextoDispositivo =
    TextoDispositivo(f(inlineSeqs))

case object OmissisSimples extends ConteudoDispositivo

final case class TituloDispositivo(inlineSeq : InlineSeq) extends HasInlineSeq[TituloDispositivo]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): TituloDispositivo =
    TituloDispositivo(f(inlineSeq))

trait DispositivoPredef extends Dispositivo:
  val tipoDispositivoPredef : TDP
  override val tipoDispositivo : TipoDispositivo = tipoDispositivoPredef

trait DispositivoNaoArtigo extends Dispositivo:
  val tipoDispositivoNaoArtigo : TipoDispositivoNaoArtigo
  override val tipoDispositivo : TipoDispositivo = tipoDispositivoNaoArtigo

final case class DispositivoPredefNA(
    id : ID,
    override val tipoDispositivo : TDP & TipoDispositivoNaoArtigo,
    titulo : Option[TituloDispositivo] = None,
    rotulo : Option[Rotulo] = None,
    conteudo : Option[ConteudoDispositivo] = None,
    alteracao : Option[Alteracao] = None,
    containers : Seq[LXContainer] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String] = None       
)  extends DispositivoPredef with DispositivoNaoArtigo:
  val tipoDispositivoPredef: TDP & TipoDispositivoNaoArtigo = tipoDispositivo
  val tipoDispositivoNaoArtigo: TDP & TipoDispositivoNaoArtigo = tipoDispositivo
  Ensuring(this).ensuring(tipoDispositivo != null, "tipoDispositivo is null!")

final case class DispositivoGenerico(
    id : ID,
    nome : String,
    titulo : Option[TituloDispositivo] = None,
    rotulo : Option[Rotulo] = None,
    conteudo : Option[ConteudoDispositivo] = None,
    alteracao : Option[Alteracao] = None,
    containers : Seq[LXContainer] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String] = None) extends DispositivoNaoArtigo:
  val tipoDispositivoNaoArtigo: TipoDispositivo & TipoDispositivoNaoArtigo = TD_Generico

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
    notaAlteracao : Option[String]) extends DispositivoPredef with HierarchicalElement:
  val tipoDispositivoPredef: TDP = TDP.Artigo
  override val tipoDispositivo: TDP = TDP.Artigo

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

sealed trait AlteracaoElement extends Product:
  val abreAspas : Boolean
  val fechaAspas : Boolean
  val notaAlteracao : Option[String]

/**
 * <xsd:element ref="FormulaPromulgacao"/>
            <xsd:element ref="Epigrafe"/>
            <xsd:element ref="Ementa"/>
            <xsd:element ref="Preambulo"/>
 */

/**
 * Inline elements    
 */

final case class Remissao(href : LexMLURN, inlineSeq : InlineSeq = InlineSeq()) extends LXInlineElement with HasInlineSeq[Remissao]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): Remissao = Remissao(href,f(inlineSeq))

final case class RemissaoMultipla(base : LexMLURN,inlineSeq : InlineSeq = InlineSeq()) extends LXInlineElement with HasInlineSeq[RemissaoMultipla]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): RemissaoMultipla = RemissaoMultipla(base,f(inlineSeq))

@unused
final case class Formula(
    mathmlElem : scala.xml.Elem) extends LXInlineElement

final case class EmLinha(nome : String, inlineSeq : InlineSeq = InlineSeq()) extends InlineElement with HasInlineSeq[EmLinha]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): EmLinha =
    EmLinha(nome,f(inlineSeq))

/**
 * Blocos
 */

sealed trait BlockElement extends AlteracaoElement

@unused
final case class ConteudoExterno(
    conteudo : scala.xml.NodeSeq = scala.xml.NodeSeq.Empty) extends BlockElement:
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None

final case class Bloco(
    nome : String,
    inlineSeq : InlineSeq = InlineSeq()) extends BlockElement with HasInlineSeq[Bloco]:
  def mapInlineSeq(f : InlineSeq => InlineSeq): Bloco = Bloco(nome,f(inlineSeq))
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None

/**
 * Blocos HTML
 */

sealed trait HTMLBlock extends BlockElement

final case class Paragraph(
    inlineSeq : InlineSeq, 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String] = None) extends HasInlineSeq[Paragraph] with HTMLBlock with LI_Item:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): Paragraph =
      copy(inlineSeq = f(inlineSeq))

@unused
final case class HTMLList(
    tipoLista : TipoLista, 
    itens : Seq[LI] = Seq(), 
    abreAspas : Boolean = false, 
    fechaAspas : Boolean = false,
    notaAlteracao : Option[String]) extends HTMLBlock with LI_Item

final case class LI(elems : Mixed[LI_Item] = Mixed()) 

@unused
final case class Table() extends HTMLBlock:
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None

/**
 * Containers
 */

sealed trait Container extends AlteracaoElement with HasID:
  val id : ID
  val elems : Seq[BlockElement]
  val abreAspas : Boolean = false
  val fechaAspas : Boolean = false
  val notaAlteracao : Option[String] = None

@unused
final case class Div(id : ID, elems : Seq[BlockElement] = Seq()) extends Container

@unused
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

sealed trait HTMLinlineElement extends InlineElement:
  val tipoHtmlInlineElement : TipoHtmlInlineElement

final case class Anchor(
    href : LexMLURN,
    name : Option[String] = None,
    target : Option[String] = None,
    inlineSeq : InlineSeq = InlineSeq()) extends HTMLinlineElement with HasInlineSeq[Anchor]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): Anchor = copy(inlineSeq = f(inlineSeq))
  val tipoHtmlInlineElement: TipoHtmlInlineElement = THIE_Anchor

final case class Span(href : LexMLURN,
    inlineSeq : InlineSeq = InlineSeq()) extends HTMLinlineElement with HasInlineSeq[Span]:
  override def mapInlineSeq(f : InlineSeq => InlineSeq): Span = copy(inlineSeq = f(inlineSeq))
  val tipoHtmlInlineElement: TipoHtmlInlineElement = THIE_Span

enum TGHIE extends TipoHtmlInlineElement:
  case B, I, Sub, Sup, Ins, Del, Dfn

final case class GenHtmlInlineElement(
    tipoHtmlInlineElement : TGHIE,
    inlineSeq : InlineSeq = InlineSeq()) extends HTMLinlineElement with HasInlineSeq[GenHtmlInlineElement]:
  def mapInlineSeq(f : InlineSeq => InlineSeq): GenHtmlInlineElement = copy(inlineSeq = f(inlineSeq))

/**
 * LexML Markers
 */

final case class NotaReferenciada(nota : IDREF) extends InlineElement

@unused
final case class Marcador() extends InlineElement

@unused
final case class Img() extends InlineElement

