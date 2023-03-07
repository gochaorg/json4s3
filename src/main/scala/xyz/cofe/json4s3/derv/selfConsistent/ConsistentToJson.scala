package xyz.cofe.json4s3.derv
package selfConsistent

trait ConsistentToJson:
  given doubleToJson:ToJson[Double] = selfConsistent.doubleToJson
  given floatToJson:ToJson[Float] = selfConsistent.floatToJson
  given intToJson:ToJson[Int] = selfConsistent.intToJson
  given shortToJson:ToJson[Short] = selfConsistent.shortToJson
  given byteToJson:ToJson[Byte] = selfConsistent.byteToJson
  given booleanToJson:ToJson[Boolean] = selfConsistent.booleanToJson
  given stringToJson:ToJson[String] = selfConsistent.stringToJson
  given listToJson[A:ToJson]:ToJson[List[A]] = selfConsistent.listToJson
  given optionToJson[A:ToJson]:ToJson[Option[A]] = selfConsistent.optionToJson

