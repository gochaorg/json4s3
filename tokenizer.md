Lexem parse (tokenizer)
========================

- _Какие есть лексемы_
- _Парсер лексем_
- _Итератор по лексемам_
- _Вложенный итератор по лексемам_


```scala
assert(TokenIterator("123 true false").toList == List(
  Token.IntNumber(123),
  Token.WhiteSpace(" "),
  Token.Identifier("true"),
  Token.WhiteSpace(" "),
  Token.Identifier("false"),
))
```

Какие есть лексемы
---------------------

`enum Token:`

- `case Str( val text:String )` - представляет строку
- `case IntNumber( val num:Int )` - представляет число - целое
- `case BigNumber( val num:BigInt )` - представляет число - большое целое
- `case FloatNumber( val num:Double )` - представляет число - плавующее
- `case OpenSquare` - квадратная скобка `[`
- `case CloseSquare` - квадратная скобка `]`
- `case OpenBrace` - фигурная скобка `{`
- `case CloseBrace` - фигурная скобка `}`
- `case Comma` - запятая `,`
- `case Colon` - двоеточие `:`
- `case WhiteSpace( val text:String )` - пробелный символ
- `case Identifier( val text:String )` - идентификатор - имеется виду `true` | `false` | `null`
- `case SLComment( val text:String )` - однострочный коментарий 
- `case MLComment( val text:String )` - многострочный коментарий

Парсер лексем
----------------

Основной парсер лексем - `xyz.cofe.json4s3.stream.token.Tokenizer`

Основной метод парсинга

```scala
def accept(state:State, char:Char):Either[TokenError,(State,List[Token])]
```

- *Параметры*
  - **state** - текущее состояние парсера
  - **char** - входящий символ
- *Результат*
  - Или
    - Ошибка
    - (новое состояние, распознаные лексемы)

Парсинг двух символов

```scala
test("sample") {
  import xyz.cofe.json4s3.stream.token.Tokenizer.{accept => tokenize}
  import xyz.cofe.json4s3.stream.token.Token
  import xyz.cofe.json4s3.stream.token.Tokenizer.State

  val parse0Et = tokenize(
    State.Init, // Начальное состояние
    '{'         // Входной символ
  )
  assert(parse0Et.isRight) 

  parse0Et.foreach { case(newState,tokens) => 
    assert(tokens == List(Token.OpenBrace)) // Распознаная лексема
    tokenize(newState,'}').foreach { case(newState,tokens) => 
      assert(tokens == List(Token.CloseBrace)) // Распознаная лексема
    }
  }
}
```

Парсинг строки - второй пример

```scala
import xyz.cofe.json4s3.stream.token.Tokenizer.{accept => tokenize}
import xyz.cofe.json4s3.stream.token.Token
import xyz.cofe.json4s3.stream.token.Tokenizer.State

// Начальное состояние
var parserState = State.Init

// Завршение цикла обработки строки
var stop = false

// Входная строка
var chars = "{} []:, 12 true 'string'".toList

// Обработка входной строки
while !chars.isEmpty && !stop do
  tokenize(parserState,chars.head) match
    case Left(err) => 
      // Ошибка входной последовательности
      println(s"catch error $err")
      stop = true
    case Right((newState,tokens)) =>
      // Очередное состояние парсера
      println(s"input char '${chars.head}' input.state=${parserState}")          
      println(s"  output state  = ${newState}")
      println(s"  output tokens = ${tokens}")

      // меняем состояние парсера
      parserState = newState

      // откидываем обработанный символ 
      chars = chars.tail
```

Вывод

    input char '{' input.state=Init
      output state  = Init
      output tokens = List(OpenBrace)
    input char '}' input.state=Init
      output state  = Init
      output tokens = List(CloseBrace)
    input char ' ' input.state=Init
      output state  = WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@64e286c,Work( ))
      output tokens = List()
    input char '[' input.state=WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@64e286c,Work( ))
      output state  = Init
      output tokens = List(WhiteSpace( ), OpenSquare)
    input char ']' input.state=Init
      output state  = Init
      output tokens = List(CloseSquare)
    input char ':' input.state=Init
      output state  = Init
      output tokens = List(Colon)
    input char ',' input.state=Init
      output state  = Init
      output tokens = List(Comma)
    input char ' ' input.state=Init
      output state  = WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@7ff70963,Work( ))
      output tokens = List()
    input char '1' input.state=WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@7ff70963,Work( ))
      output state  = NumParse(xyz.cofe.json4s3.stream.token.number$Parser@5e1e7dc1,DecPart(List(1),true))
      output tokens = List(WhiteSpace( ))
    input char '2' input.state=NumParse(xyz.cofe.json4s3.stream.token.number$Parser@5e1e7dc1,DecPart(List(1),true))
      output state  = NumParse(xyz.cofe.json4s3.stream.token.number$Parser@5e1e7dc1,DecPart(List(1, 2),true))
      output tokens = List()
    input char ' ' input.state=NumParse(xyz.cofe.json4s3.stream.token.number$Parser@5e1e7dc1,DecPart(List(1, 2),true))
      output state  = WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@62fab2e1,Work( ))
      output tokens = List(IntNumber(12))
    input char 't' input.state=WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@62fab2e1,Work( ))
      output state  = IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(t))
      output tokens = List(WhiteSpace( ))
    input char 'r' input.state=IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(tr))
      output state  = IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(tr))
      output tokens = List()
    input char 'u' input.state=IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(tru))
      output state  = IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(tru))
      output tokens = List()
    input char 'e' input.state=IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(true))
      output state  = IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(true))
      output tokens = List()
    input char ' ' input.state=IdParser(xyz.cofe.json4s3.stream.token.identifier$Parser@7e0f29bd,Work(true))
      output state  = WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@1c6799d0,Work( ))
      output tokens = List(Identifier(true))
    input char ''' input.state=WhitespaceParse(xyz.cofe.json4s3.stream.token.whitespace$Parser@1c6799d0,Work( ))
      output state  = StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',))
      output tokens = List(WhiteSpace( ))
    input char 's' input.state=StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',s))
      output state  = StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',s))
      output tokens = List()
    input char 't' input.state=StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',st))
      output state  = StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',st))
      output tokens = List()
    input char 'r' input.state=StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',str))
      output state  = StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',str))
      output tokens = List()
    input char 'i' input.state=StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',stri))
      output state  = StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',stri))
      output tokens = List()
    input char 'n' input.state=StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',strin))
      output state  = StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',strin))
      output tokens = List()
    input char 'g' input.state=StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',string))
      output state  = StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',string))
      output tokens = List()
    input char ''' input.state=StrParse(xyz.cofe.json4s3.stream.token.string$Parser@101ae6b6,SimpleChar(',string))
      output state  = Init
      output tokens = List(Str(string))

Итератор
-----------------

Создать итератор просто

```scala
assert(TokenIterator("123 true false").toList == List(
  Token.IntNumber(123),
  Token.WhiteSpace(" "),
  Token.Identifier("true"),
  Token.WhiteSpace(" "),
  Token.Identifier("false"),
))
```

### NestedTokenIterator

Особоенность любого итератора, что перед тем как получить элемент из итератора (`next()`), необходимо проверить наличие (`hasNext:Boolean`).
Из этого следует, что итератор имеет внутреннее состояние

Допустим есть такая последовательность лексем

    { "a": 1 } { "b": 2 }

Эта последовательность не является правильной с точки зрения Json, тут два объекта

    { "a": 1 } { "b": 2 }
    ▲        ▲ ▲        ▲
    |        | |        |
    |        | +--------+-- второй объект
    |        |
    +--------+------------- первый объект

Прочесть эти лексемы как `List[Object]` в рамках одного правильного парсера Json - проблематично.
Необходимо переписывать грамматику, которая будет не совместима с Json.

Но можно поступить по другому, для каждого объекта использовать свой итератор

    { "a": 1 } { "b": 2 }
    ▲        ▲ ▲        ▲
    |        | |        |
    |        | +--------+-- Прочесть одним итератором
    |        |
    +--------+------------- Прочесть другим итератором
    
Главное что бы итератор остановился после соответствующей скобки.
Эту задачу решает NestedTokenIterator

```scala
val jsonString = 
  """|{
      |  "hits" : [
      |    { "a": 1 },
      |    { "b": 2 },
      |    { "c": 3 },
      |    { "d": 4 }   
      |   ]
      |}
  """.stripMargin.replace("\n","").replaceAll(" +","")

val srcTIter = TokenIterator(jsonString)

// skip: { "hits" : [
(0 until 4).foreach { _ => srcTIter.next() }

var nestedIter = new NestedTokenIterator(srcTIter)
assert( nestedIter.toList == List( OpenBrace, Str("a"), Colon, IntNumber(1), CloseBrace ) )

// expect: ,
assert( srcTIter.next() == Comma )
nestedIter = new NestedTokenIterator(srcTIter)
assert( nestedIter.toList == List( OpenBrace, Str("b"), Colon, IntNumber(2), CloseBrace ) )

// expect: ,
assert( srcTIter.next() == Comma )
nestedIter = new NestedTokenIterator(srcTIter)
assert( nestedIter.toList == List( OpenBrace, Str("c"), Colon, IntNumber(3), CloseBrace ) )

// expect: ,
assert( srcTIter.next() == Comma )
nestedIter = new NestedTokenIterator(srcTIter)
assert( nestedIter.toList == List( OpenBrace, Str("d"), Colon, IntNumber(4), CloseBrace ) )
```