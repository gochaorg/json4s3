package xyz.cofe.json4s3

import derv.errors._
import xyz.cofe.json4s3.stream.token.Tokenizer
import xyz.cofe.json4s3.stream.ast.Parser
import xyz.cofe.json4s3.stream.ast.AST
import xyz.cofe.json4s3.stream.ast.FormattingJson

package object derv {
  extension (string:String)
    def jsonAs[A:FromJson]:Either[DervError,A] = 
      Tokenizer.parse(string).left.map(DervError.from).flatMap { tokens =>
        Parser.parseSeq(tokens).left.map(DervError.from).flatMap { case((js,tail)) => 
          summon[FromJson[A]].fromJson(js)
        }
      }

  extension [A:ToJson]( item:A )(using formatting:FormattingJson)
    def asJson:Option[AST] = 
      summon[ToJson[A]].toJson(item)
    def json:String =
      summon[ToJson[A]].toJson(item).map(_.json).getOrElse("")
}
