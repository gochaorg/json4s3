package xyz.cofe.json4s3.stream.token

import java.io.Reader
import scala.util.Try
import scala.util.Failure
import scala.util.Success

/** Итератор по токнам */
// class TokenIteratorEt(
//   private var tokenizer: Tokenizer,
//   private var state: Tokenizer.State,
//   private val reader: Reader
// ) extends Iterator[Either[String,Token]]:

//   private def fetch(state:Tokenizer.State, reader:Reader) =
//     var stop = false
//     while !stop do
//       Try( reader.read().toChar ) match
//         case Failure(exception) => 
//         case Success(value) =>
      
//   override def hasNext: Boolean = ???
//   override def next(): Either[String, Token] = ???
