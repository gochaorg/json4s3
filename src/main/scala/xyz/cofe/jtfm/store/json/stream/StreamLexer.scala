package xyz.cofe.jtfm.store.json.stream

/** Парсер лексемы */
trait StreamTokenParser[CHAR]:
  /** Состояние парсера */
  type STATE <: StreamTokenParserState
  /** Лексема */
  type OUT <: Token
  /** Чтение очередного символа */
  def accept(state:STATE,char:CHAR):STATE

  /** Завершение данных */
  def end(state:STATE):STATE

  /** сброс состояния */
  def init:STATE

  /** Чтение уже готовых лексем */
  def ready(state:STATE):Option[OUT]
  /** Чтение лексем которые еще не готовы */
  def tail(state:STATE):Option[OUT]

trait StreamTokenParserState:
  /** состояние ошибки входных данных */
  def isError:Boolean

  /** входные данные принимаются */
  def isAcceptable:Boolean

  /** есть распознаные данные */
  def isReady:Boolean

  /** 
   * входной символ принадлежит текущему состоянию 
   * 
   * сценарий
   * 
   * правило  id ::= char {char}
   * 
   * в данном правиле нет терминального символа
   * 
   * входная последовательность
   * 
   * входной символ
   *   a  
   *   b
   *   "
   * 
   * правило str ::= " {char} "
   * 
   * в данном правиле есть терминальный символ
   * 
   * входная последовательность
   * 
   * входной символ
   * 
   *   "
   *   a  
   *   b
   *   "
   * 
   */
  def isConsumed:Boolean

