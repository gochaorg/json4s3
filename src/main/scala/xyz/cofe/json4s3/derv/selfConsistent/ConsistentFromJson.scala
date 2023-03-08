package xyz.cofe.json4s3.derv
package selfConsistent

trait ConsistentFromJson:
  given doubleFromJson:FromJson[Double] = selfConsistent.doubleFromJson
  given floatFromJson:FromJson[Float] = selfConsistent.floatFromJson
  given intFromJson:FromJson[Int] = selfConsistent.intFromJson
  given shortFromJson:FromJson[Short] = selfConsistent.shortFromJson
  given byteFromJson:FromJson[Byte] = selfConsistent.byteFromJson
  given booleanFromJson:FromJson[Boolean] = selfConsistent.booleanFromJson
  given stringFromJson:FromJson[String] = selfConsistent.stringFromJson
  //given listFromJson[A:FromJson]:FromJson[List[A]] = selfConsistent.listFromJson
  given optionFromJson[A:FromJson]:FromJson[Option[A]] = selfConsistent.optionFromJson
