package xyz.cofe.json4s3.derv

/** 
 * Значение по умолчанию, используется при восстановлении из Json
 * 
 * Пример
 * {{{
 * case class Append(value:String)
 * object Append:
 *  given DefaultValue[Append] with
 *    override def defaultValue: Option[Append] = Some(Append("def_value"))
 * }}}
 * 
 * Еще пример
 * 
 * {{{
 *   case class DefVal( a:String )
 *  object DefVal:
 *    given defval:DefaultValue[DefVal] = new DefaultValue[DefVal] {
 *      override def defaultValue: Option[DefVal] = Some(DefVal("sample"))
 *    }
 *  case class ItemWithDef( a:Int, b:DefVal )
 *
 *  test("default") {
 *    assert("""{ "a": 1 }"""
 *      .jsonAs[ItemWithDef] == Right(ItemWithDef(1,DefVal("sample"))))
 *  }
 * }}}
 */
trait DefaultValue[T]:
  def defaultValue:Option[T]

object DefaultValue:
  given [T]: DefaultValue[Option[T]] with
    def defaultValue: Option[Option[T]] = Some(None)

  given [T]: DefaultValue[T] with
    def defaultValue: Option[T] = None

