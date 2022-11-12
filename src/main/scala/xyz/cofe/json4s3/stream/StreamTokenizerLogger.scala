package xyz.cofe.json4s3.stream

trait StreamTokenizerLogger:
  def apply(message:String):Unit
  def apply(code: =>Unit):Unit

object StreamTokenizerLogger:
  given StreamTokenizerLogger with
    def apply(message: String): Unit = ()
    def apply(code: =>Unit):Unit = ()
  def stdout:StreamTokenizerLogger = new StreamTokenizerLogger:
    def apply(message: String): Unit = println(message.replace("\n","\\n").replace("\r","\\r"))
    def apply(code: =>Unit):Unit = code