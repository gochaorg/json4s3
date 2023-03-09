package xyz.cofe.json4s3.desc

import xyz.cofe.json4s3.stream.ast.AST
import JsType._

/** Создает описание типа - см [[JsType]] */
object JsonDescribe:
  def describe( ast:AST ):JsType =
    ast match
      case AST.JsStr(value) => JsStr
      case AST.JsFloat(value) => JsFloat
      case AST.JsInt(value) => JsInt
      case AST.JsBig(value) => JsBig
      case AST.JsNull => JsNull
      case AST.JsBool(value) => JsBool
      case AST.JsArray(value) => JsArray
      case AST.JsObj(fields) =>         
        Prod( fields.map((name,fld) => name -> describe(fld)).toMap )

