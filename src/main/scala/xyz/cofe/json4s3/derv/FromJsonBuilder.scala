package xyz.cofe.json4s3.derv

import xyz.cofe.json4s3.query.Query
import xyz.cofe.json4s3.query.errors

object FromJsonBuilder:
  def query[A]:Selector[A] = ???

  class Selector[A]:
    def select[B<:A:FromJson](query:Query=>Boolean):Selector[A] = ???
    def build:FromJson[A] = ???

    def fetch( init: Fether ?=> Unit ):Selector[A] = ???

  extension [A,B](query:Either[errors.QueryError,String])
    def ===( value:String ):Boolean = ???

  class Fether

  def select[A](using f:Fether):Query = ???
