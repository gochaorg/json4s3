package xyz.cofe.json4s3.derv

object OptionExt:
  extension[T] (optValue:Option[T])
    def lift[E](errMessage:E):Either[E,T] = optValue match
      case Some(v) => Right(v)
      case None => Left(errMessage)  
