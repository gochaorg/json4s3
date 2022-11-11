package xyz.cofe.jtfm.store.json.stream

trait StreamTokenizerLogger:
  def apply(message:String):Unit
  def apply(code: =>Unit):Unit

object StreamTokenizerLogger:
  given StreamTokenizerLogger with
    def apply(message: String): Unit = ()
    def apply(code: =>Unit):Unit = ()
  def stdout:StreamTokenizerLogger = new StreamTokenizerLogger:
    def apply(message: String): Unit = println(message)
    def apply(code: =>Unit):Unit = code