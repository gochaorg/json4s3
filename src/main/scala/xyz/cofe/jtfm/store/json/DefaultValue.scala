package xyz.cofe.jtfm.store.json

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
 */
trait DefaultValue[T]:
  def defaultValue:Option[T]

object DefaultValue:
  given [T]: DefaultValue[T] with
    def defaultValue: Option[T] = None