package xyz.cofe.json4s3

import derv.errors._
import xyz.cofe.json4s3.stream.token.Tokenizer
import xyz.cofe.json4s3.stream.ast.Parser

package object derv {
  extension (string:String)
    def jsonAs[A:FromJson]:Either[DervError,A] = 
      Tokenizer.parse(string).left.map(DervError.from).flatMap { tokens =>
        Parser.parse(tokens).left.map(DervError.from).flatMap { case((js,tail)) => 
          summon[FromJson[A]].fromJson(js)
        }
      }

  
}
