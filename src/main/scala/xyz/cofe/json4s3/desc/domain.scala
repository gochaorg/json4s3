package xyz.cofe.json4s3.desc

/** 
 * Описывает Json тип
 * 
 * Есть простые типы:
 *
 * - JsNull - соответ литералу null
 * - JsStr - соответ строковому литералу
 * - JsFloat
 * - JsInt
 * - JsBig
 * - JsBool
 * 
 * Составные типы
 * - JsArray - массив
 * - Prod - "тип-произведение"
 *   - тип-произведение - это тип к элементу которого можно обратиься по ключу
 *   - приминительно к JSON это может быть:
 *     - массив, где ключ - это цело число
 *     - объект, где ключ - это имя/идентификатор поля объекта
 * - Sum  - "тип-сумма"
 *   - тип-сумма - это тип который может быть либо типом А, либо типом Б, либо В ....
 *     опеределить какого именного типа возможно по функции того или иного языка (например в js это функция typeof() / а для java - оператор instanceOf ) 
 * 
 * Пример:
 * {{{
 *  val desc =
 *   // исследуемые json
 *   List(
 *     """{ "a": 1, "b":2 }""",
 *     """{ "a": 123, "c":"abc" }""",
 *     """{ "a": true, "d":[1,2], "b": { "x": 1 } }""",
 *   ).map( Parser.parse )
 *   .map( _.toOption )
 *   .flatten // Получаем тип List[AST]
 *   .foldLeft( 
 *     Prod(Map.empty):JsType // Теперь каждый тип ast будет объеденен с данным
 *   ){ case (jsType,ast) => jsType.merge(JsonDescribe.describe(ast)) } // объединение типов
 * }}}
 * 
 * в результате будет такое
 * 
 * ```
 * {
 *   a : Sum(
 *     2 > JsInt
 *     1 > JsBool 
 *   )
 *   b : Sum(
 *     1 > JsInt
 *     1 > {      
 *       x : JsInt      
 *     } 
 *   )
 *   c : JsStr
 *   d : JsArray
 * }
 * ```
 * 
 * - `{}` - обозначер Prod тип ("тип-произведение")
 * - ''имя'' : ''тип'' - описывает тип поля объекта (prod)
 * - `Sum()` - обозначер Sum тип
 * - ''число'' > ''тип'' - указывает на элемент типа-суммы и сколько данный тип встретился
 *     - для типа-произведения число всегда будет 1
 */
sealed trait JsType:
  def merge( t:JsType ):JsType

object JsType:
  case class ToStr(level:Int)

  case object JsNull extends JsType:
    override def merge(t: JsType): JsType = 
      t match
        case s @  Sum(types) => s.merge(JsNull)
        case _ => Sum( Map() ).merge(JsNull).merge(t)
    override def toString(): String = "JsNull"
      
  case object JsStr extends JsType:
    override def merge(t: JsType): JsType = 
      t match
        case s @  Sum(types) => s.merge(JsStr)
        case _ => Sum( Map() ).merge(JsStr).merge(t)
    override def toString(): String = "JsStr"

  case object JsFloat extends JsType:
    override def merge(t: JsType): JsType = 
      t match
        case s @  Sum(types) => s.merge(JsFloat)
        case _ => Sum( Map() ).merge(JsFloat).merge(t)
    override def toString(): String = "JsFloat"

  case object JsInt extends JsType:
    override def merge(t: JsType): JsType = 
      t match
        case s @  Sum(types) => s.merge(JsInt)
        case _ => Sum(Map()).merge(JsInt).merge(t)
    override def toString(): String = "JsInt"

  case object JsBig extends JsType:
    override def merge(t: JsType): JsType = 
      t match
        case s @  Sum(types) => s.merge(JsBig)
        case _ => Sum(Map()).merge(JsBig).merge(t)
    override def toString(): String = "JsBig"

  case object JsBool extends JsType:
    override def merge(t: JsType): JsType = 
      t match
        case s @  Sum(types) => s.merge(JsBool)
        case _ => Sum(Map()).merge(JsBool).merge(t)
    override def toString(): String = "JsBool"
        
  case object JsArray extends JsType:
    override def merge(t: JsType): JsType = 
      t match
        case s @  Sum(types) => s.merge(JsArray)
        case _ => Sum(Map()).merge(JsArray).merge(t)
    override def toString(): String = "JsArray"
        
  case class Sum( types:Map[JsType,Int] ) extends JsType:
    override def hashCode(): Int = 1
    override def equals(x: Any): Boolean = x.isInstanceOf[Sum]
    override def merge(t: JsType): JsType =
      t match
        case Sum(otherTypes) =>
          var tmap = Map.empty[JsType,Int]

          val cNull = types.getOrElse(JsNull,0) + otherTypes.getOrElse(JsNull,0)
          if cNull>0 then tmap = tmap + (JsNull -> cNull)

          val cStr = types.getOrElse(JsStr,0) + otherTypes.getOrElse(JsStr,0)
          if cStr>0 then tmap = tmap + (JsStr -> cStr)

          val cFloat = types.getOrElse(JsFloat,0) + otherTypes.getOrElse(JsFloat,0)
          if cFloat>0 then tmap = tmap + (JsFloat -> cFloat)

          val cInt = types.getOrElse(JsInt,0) + otherTypes.getOrElse(JsInt,0)
          if cInt>0 then tmap = tmap + (JsInt -> cInt)

          val cBig = types.getOrElse(JsBig,0) + otherTypes.getOrElse(JsBig,0)
          if cBig>0 then tmap = tmap + (JsBig -> cBig)

          val cBool = types.getOrElse(JsBool,0) + otherTypes.getOrElse(JsBool,0)
          if cBool>0 then tmap = tmap + (JsBool -> cBool)

          val cArr = types.getOrElse(JsArray,0) + otherTypes.getOrElse(JsArray,0)
          if cArr>0 then tmap = tmap + (JsArray -> cArr)

          val obj0opt =      types.find((t,_)=>t.isInstanceOf[Prod]).map((t,c) => (t.asInstanceOf[Prod],c))
          val obj1opt = otherTypes.find((t,_)=>t.isInstanceOf[Prod]).map((t,c) => (t.asInstanceOf[Prod],c))

          if obj0opt.isDefined && obj1opt.isDefined
          then 
            val (obj0,c0) = obj0opt.get
            val (obj1,c1) = obj1opt.get
            tmap = tmap + ( obj0.merge( obj1 ) -> (c0 + c1) )
          else 
            if obj0opt.isDefined
            then 
              val (obj0,c0) = obj0opt.get
              tmap = tmap + ( obj0 -> c0 )
            else 
              if obj1opt.isDefined
              then
                val (obj1,c1) = obj1opt.get
                tmap = tmap + ( obj1 -> c1 )

          otherTypes.find( (t,_) => t.isInstanceOf[Sum] ).map( (t,_) => t.asInstanceOf[Sum] ).map { sum => 
            sum.merge( Sum(tmap) )
          }.getOrElse( Sum(tmap) )

        case _ => 
          Sum( types + (t -> (types.get(t).getOrElse(0) + 1)) )
          
    override def toString(): String = 
      if types.size==1 
      then
        val innerLines = types.map( (t,c) => 
          val lines = t.toString().split("\n")
          if lines.size==1 
          then 
            lines.head + s"($c)"
          else
            val pref = s"$c > "
            pref + lines.mkString(" "*(pref.length()+2)+"\n")
        ).toList
        if innerLines.size==1
        then
          innerLines.head.trim()
        else
          s"""|Sum(
              |${innerLines.map(l=>"  "+l).mkString("\n")} 
              |)
          """.stripMargin
      else
        val inner = types.map( (t,c) => 
          val pref = s"$c > "
          val lines = t.toString().split("\n")
          pref + lines.mkString(" "*(pref.length()+2)+"\n")
        ).mkString("\n")

        val innerLines = inner.split("\n")
        if innerLines.size==1 then
          s"Sum( ${innerLines.head.trim} )"
        else
          s"""|Sum(
              |${innerLines.map(l=>"  "+l).mkString("\n")} 
              |)
          """.stripMargin
      
  case class Prod( items:Map[String|Int, JsType] ) extends JsType:
    override def hashCode(): Int = 2
    override def equals(x: Any): Boolean = x.isInstanceOf[Prod]

    def mergeProd(t:Prod):Prod =
      val otherItems = t.items
      var tmap = Map.empty[String|Int, JsType]  
      items.foreach { case (k,t0) =>
        otherItems.get(k) match
          case None => 
            tmap = tmap + (k -> t0)
          case Some(t1) =>
            tmap = tmap + (k -> t0.merge(t1))
      }
      otherItems.filter { case (k,t) => ! items.keySet.contains(k) }.foreach( (k,t) => tmap = tmap + (k -> t) )
      Prod(tmap)

    override def merge(t: JsType): JsType = 
      t match
        case p @ Prod(otherItems) =>
          mergeProd(p)
        case Sum(types) =>
          val objOptEt = types.partitionMap( (t,c)=>
            if t.isInstanceOf[Prod] 
            then Left((t.asInstanceOf[Prod],c))
            else Right((t,c))
          )
          val (objs,others) = objOptEt
          val objMerged = objs.toList.foldLeft( (this,1) ){ case ((obj,c0),(other,c1)) => 
            (obj.mergeProd(other), c0+c1)
          }
          val tmap = Map[JsType,Int]( objMerged ) ++ others.toMap
          Sum(tmap)
        case _ =>
          Sum( Map(this->1, t->1) )
    
    override def toString(): String = 
      val lines = items.toList.sortBy((k,_)=>k.toString()).map( (k,t) => 
        (s"$k : $t").split("\n").map(line => "  " + line).mkString("\n")
      )
      val inner = lines.mkString("\n").split("\n").flatMap { line => 
        if line.trim().isEmpty() then List.empty else List(line)
      }.mkString("\n")

      s"""|{
          |$inner
          |}
          |""".stripMargin

  object Prod:
    val empty:Prod = Prod(Map.empty)

