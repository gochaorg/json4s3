package xyz.cofe.jtfm.store.json

/** 
 * Указатель на позицию токена в списке токенов
 */
case class LPtr( val value:Int, val source:Seq[Token] ):
  import scala.reflect._

  /** Возвращает текущий токен */
  def token:Option[Token]=
    val t = value
    if t>=0 && t<source.size then
      Some(source(value))
    else
      None

  /** Указатель на начало в позиции в тексте */
  def beginPtr:Ptr = token.map(_.begin).getOrElse(Ptr(-1,""))

  /** Указатель на конец в позиции в тексте */
  def endPtr:Ptr =  token match
    case Some(existsToken) => existsToken.end
    case None => (this + (-1)).token match
      case Some(preEndToken) => 
        preEndToken.end
      case None => throw new 
        RuntimeException(s"can't compute end ptr of ${this}")
      
  /** Возвращает токен относительно указателя */
  def token(off:Int):Option[Token]=
    val t = value+off
    if t>=0 && t<source.size then
      Some(source(value+off))
    else
      None

  /** Возвращает токен указанного типа относительно текущего указателя */
  def fetch[T<:Token:ClassTag](off:Int):Option[T]=
    val ct = summon[ClassTag[T]]
    val t = value+off
    if t>=0 && t<source.size then
      val x = source(value+off)
      ct.unapply(x)
    else
      None

  /** Проверяет что указатель находиться в пределах списка */
  def inside: Boolean = !empty

  /** Проверяет что указатель вышел за предела списка */
  def empty: Boolean = value < 0 || value >= source.size

  /** создает новый указатель со смещением от текущего */
  def +( off:Int ):LPtr = copy( value = value+off )

  /** проверяет что указывает на идентификатор определенного типа */
  def isIdentifier(off:Int,txtPred:String=>Boolean):Option[Token.Identifier]=
    fetch[Token.Identifier](off).flatMap(t => if txtPred(t.text) then Some(t) else None )

  /** проверяет что указыавет на идентификатор null */
  def isNull(off:Int)  = isIdentifier(off, _=="null")

  /** проверяет что указыавет на идентификатор null */
  def isFalse(off:Int) = isIdentifier(off, _=="false")

  /** проверяет что указыавет на идентификатор null */
  def isTrue(off:Int)  = isIdentifier(off, _=="true")

