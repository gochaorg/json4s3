package xyz.cofe.json4s3.stream.ast

/**
  * Настройки форматирования
  */
trait FormattingJson:
  /** приличное форматирование, с отступами как пологается */
  def pretty:Boolean

  /** перевод строки */
  def endline:String

  /** отступ */
  def indent:String

  def beforeColon:String
  def afterColon:String
  def commaSpace:String

object FormattingJson:
  given FormattingJson with
    def pretty: Boolean = false
    def endline: String = "\n"
    def indent: String = "  "
    def beforeColon = ""
    def afterColon = ""
    def commaSpace = ""

  case class FormatBuilder( 
    pretty:Boolean, 
    endline:String,
    indent:String,
    beforeColon:String,
    afterColon:String,
    commaSpace:String,
  ) extends FormattingJson:
    //def pretty:Boolean = this.pretty
    def pretty(v:Boolean):FormatBuilder = copy(pretty = v)
    def endline(str:String):FormatBuilder = copy(endline = str)
    def indent(str:String):FormatBuilder = copy(indent = str)
    def beforeColon(str:String):FormatBuilder = copy(beforeColon = str)
    def afterColon(str:String):FormatBuilder = copy(afterColon = str)
    def commaSpace(str:String):FormatBuilder = copy(commaSpace = str)
  
  def formatting:FormatBuilder = FormatBuilder(
    pretty = false, 
    endline = "\n", 
    indent = "  ",
    beforeColon = "",
    afterColon = "",
    commaSpace = ""
  )
  def pretty( v:Boolean ) = formatting.pretty(v)

  