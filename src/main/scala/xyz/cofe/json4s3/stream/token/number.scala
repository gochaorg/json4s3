package xyz.cofe.json4s3.stream.token

/** Распознование чисел 
 * 
 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Grammar_and_types
 * 
 * = number types =
 * == float point ==
 * 
 * `[digits].[digits][(E|e)[(+|-)]digits]`
 * 
 * ```
 * 3.1415926
 * .123456789
 * 3.1E+12
 * .1e-23
 * ```
 * 
 * == integer ==
 * 
 * ```
 * 0, 117, 123456789123456789n             (decimal, base 10)
 * 015, 0001, 0o777777777777n              (octal, base 8)
 * 0x1123, 0x00111, 0x123456789ABCDEFn     (hexadecimal, "hex" or base 16)
 * 0b11, 0b0011, 0b11101001010101010101n   (binary, base 2)
 * ```
 * = grammar =
 *
 *
 *     number      ::= [ unary_minus ] integer | float
 *     
 *     integer     ::= octal_int | hex_int | bin_int | dec_int
 *     octal_int   ::= '0' [ 'o' | 'O' ] { octal_digit } [ 'n' ]
 *       hex_int   ::= '0' ( 'x' | 'X' ) { hex_digit } [ 'n' ]
 *       bin_int   ::= '0' ( 'b' | 'B' ) { bin_digit } [ 'n' ]
 *       dec_int   ::= dec_digit { dec_digit } [ 'n' ]
 *     
 *       bin_digit ::= '0' | '1'
 *     octal_digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
 *       dec_digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
 *       hex_digit ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' 
 *                   | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'
 *                   | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
 *     
 * float         ::= dec_part '.' fraction_part ( 'e' | 'E' ) [ '-' | '+' ] exponent_part
 *                 | dec_part '.' fraction_part 
 *                 | dec_part '.' ( 'e' | 'E' )  [ '-' | '+' ] exponent_part 
 *                 | dec_part '.'
 *                 | '.' fraction_part ( 'e' | 'E' )  [ '-' | '+' ] exponent_part
 *                 | '.' fraction_part 
 *      
 * dec_part      ::= dec_digit { dec_digit }
 * fraction_part ::= dec_digit { dec_digit }
 * exponent_part ::= dec_digit { dec_digit }
 */
object number:
  /** Модель состояний автомата */
  enum State extends StreamTokenParserState:
    // '-' -> DecStart
    // '0' -> DecPref
    // '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> DecPart
    // '.' -> FloatExpectFraction
    // -> Err
    case Init extends State

    // '0' -> DecPref
    // '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> DecPart
    // '.' -> FloatExpectFraction
    // -> Err
    case DecStart( positive:Boolean=true ) extends State

    // 'x' | 'X' -> HexInt
    // 'b' | 'B' -> BinInt
    // 'o' | 'O' -> OctInt
    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' -> OctInt
    // -> Finish
    case DecPref( positive:Boolean=true ) extends State

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> DecPart
    // 'n' -> FinishConsumed
    // '.' -> FloatAfterPoint
    // _ -> Finish
    case DecPart( digits:List[Int]=List(), positive:Boolean=true ) extends State

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    //     | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' 
    //     | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' -> HexInt
    // 'n' -> FinishConsumed
    // -> Finish
    case HexInt( digits:List[Int]=List(), positive:Boolean=true ) extends State

    // '0' | '1' -> BinInt
    // 'n' -> FinishConsumed
    // -> Finish
    case BinInt( digits:List[Int]=List(), positive:Boolean=true ) extends State

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' -> OctInt
    // 'n' -> FinishConsumed
    // -> Finish
    case OctInt( digits:List[Int]=List(), positive:Boolean=true ) extends State

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> FloatFraction
    // 'e' | 'E' -> ExpoSignOpt
    // -> Finish
    case FloatAfterPoint( dec:List[Int]=List(), positive:Boolean=true ) extends State

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> FloatFraction
    // 'e' | 'E' -> ExpoSignOpt
    // -> Finish
    case FloatFraction( dec:List[Int]=List(), fraction:List[Int]=List(), positive:Boolean=true ) extends State

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> FloatFraction
    // -> Err
    case FloatExpectFraction( dec:List[Int]=List(), positive:Boolean=true ) extends State

    // '+' | '-' -> Expo
    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->  Expo
    // -> Err
    case ExpoSignOpt( dec:List[Int]=List(), fraction:List[Int]=List(), positive:Boolean=true ) extends State

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->  Expo
    // -> Finish
    case Expo( dec:List[Int]=List(), fraction:List[Int]=List(), expo:List[Int], expoPositive:Boolean=true, positive:Boolean=true ) extends State

    case Finish( 
      base:Int, 
      float:Boolean,
      big:Boolean, 
      dec:List[Int]=List(), 
      fraction:List[Int]=List(), 
      expo:List[Int]=List(), 
      expoPositive:Boolean=true, 
      positive:Boolean=true 
    ) extends State
    case FinishConsumed( 
      base:Int, 
      float:Boolean, 
      big:Boolean, 
      dec:List[Int]=List(), 
      fraction:List[Int]=List(), 
      expo:List[Int]=List(), 
      expoPositive:Boolean=true, 
      positive:Boolean=true 
    ) extends State
    case Err extends State

    override def isAcceptable: Boolean = this match
      case State.Err => false
      case _:State.Finish => false
      case _ => true
    
    override def isReady: Boolean = this match
      case _:State.Finish => true
      case _:State.FinishConsumed => true
      case _:State.Expo => true
      case _:State.ExpoSignOpt => true
      case _:State.FloatExpectFraction => true
      case _:State.FloatFraction => true
      case _:State.FloatAfterPoint => true
      case _:State.OctInt => true
      case _:State.BinInt => true
      case _:State.HexInt => true
      case _:State.DecPart => true
      case _ => false
    
    override def isError: Boolean = this match
      case State.Err => true
      case _ => false
    override def isConsumed: Boolean = this match
      case _:State.FinishConsumed => true
      case _ => false

  class Parser extends StreamTokenParser[Char]:
    override type STATE = State
    override type OUT = Token

    override def init: State = State.Init

    override def ready(state: State): Option[Token] = 
      end(state) match
        case State.Finish(base, float, big, dec, fraction, expo, expoPositive, positive) =>
          build(base, float, big, dec, fraction, expo, expoPositive, positive)
        case State.FinishConsumed(base, float, big, dec, fraction, expo, expoPositive, positive) =>
          build(base, float, big, dec, fraction, expo, expoPositive, positive)
        case _ => None

    private def build(
      base:Int, 
      float:Boolean,
      big:Boolean, 
      dec:List[Int]=List(), 
      fraction:List[Int]=List(), 
      expo:List[Int]=List(), 
      expoPositive:Boolean=true, 
      positive:Boolean=true      
    ):Option[Token] = float match
      case true => Some(Token.FloatNumber(
        // floatValue(dec, fraction, expo, expoPositive, positive)
        bigDecValue(dec, fraction, expo, expoPositive, positive).toDouble
      ))
      case false => big match
        case true => Some(Token.BigNumber(bigIntValue(
          base, dec, positive
        )))
        case false => Some(Token.IntNumber(intValue(
          base, dec, positive
        )))

    private def decValue[N:Numeric](
      base:N,
      digits:List[Int],
      positive:Boolean,
      one:N,
      minusOne:N,
      digitOf:Int=>N,
    ):N = {
      import Numeric.Implicits._
      digits.reverse.zipWithIndex.map { case(digit,dIndex) => 
        (digit, {
          dIndex match
            case 0 => one
            case 1 => base
            case _ => (1 until dIndex).foldLeft(base)( (sum,_) => sum * base )
        })
      }.map( (digit, kof) => digitOf(digit) * kof ).sum * ( if positive then one else minusOne )
    }

    private def fractionValue[N:Numeric](
      base:N,
      digits:List[Int],
      positive:Boolean,
      one:N,
      minusOne:N,
      digitOf:Int=>N,
    ):N = {
      import Numeric.Implicits._
      digits.reverse.zipWithIndex.map { case(digit,dIndex) => 
        (digit, {
          dIndex match
            case 0 => base
            case _ => (0 until dIndex).foldLeft(base)( (sum,_) => sum * base )
        })
      }.map( (digit, kof) => digitOf(digit) * kof
      ).sum * ( if positive then one else minusOne )
    }

    private def bigIntValue(
      baseInt:Int,
      digits:List[Int],
      positive:Boolean
    ):BigInt = {
      decValue(BigInt(baseInt),digits,positive,BigInt(1),BigInt(-1),BigInt(_))
    }

    private def intValue(
      base:Int,
      digits:List[Int],
      positive:Boolean
    ):Int = 
      decValue(base,digits,positive,1,-1,n=>n)

    private def floatValue(
      dec:List[Int],
      fraction:List[Int], 
      expo:List[Int], 
      expoPositive:Boolean, 
      positive:Boolean
    ):Double =
      val decPart = decValue(10.0, dec, positive, 1.0, -1.0, d => d.toDouble)
      val frcPart = fractionValue(0.1,fraction,positive,1.0,-1.0,d=>d.toDouble)
      val baseValue = decPart + frcPart

      if expo.isEmpty then
        baseValue
      else 
        val expInt = decValue(10, expo, true, 1, -1, d=>d )
        val exp = (if( expoPositive )
                    {expInt match
                      case 0 => 1.0
                      case 1 => 10.0
                      case _ => (1 until expInt).foldLeft(10.0)((sum,_)=>sum*10.0)}
                  else
                    {expInt match
                      case 0 => 1.0
                      case 1 => 0.1
                      case _ => (1 until expInt).foldLeft(0.1)((sum,_)=>sum*0.1)})
        baseValue * exp

    private def bigDecValue(
      dec:List[Int],
      fraction:List[Int], 
      expo:List[Int], 
      expoPositive:Boolean, 
      positive:Boolean
    ):BigDecimal =
      val decPart = decValue(BigDecimal(10.0), dec, positive, BigDecimal(1.0), BigDecimal(-1.0), d => BigDecimal(d))
      val frcPart = fractionValue(BigDecimal(0.1),fraction,positive,BigDecimal(1.0),BigDecimal(-1.0),d=>BigDecimal(d))
      val baseValue = decPart + frcPart

      if expo.isEmpty then
        baseValue
      else 
        val expInt = decValue(10, expo, true, 1, -1, d=>d )
        val exp = (if( expoPositive )
                    {expInt match
                      case 0 => BigDecimal(1.0)
                      case 1 => BigDecimal(10.0)
                      case _ => (1 until expInt).foldLeft(BigDecimal(10.0))((sum,_)=>sum*BigDecimal(10.0))}
                  else
                    {expInt match
                      case 0 => BigDecimal(1.0)
                      case 1 => BigDecimal(0.1)
                      case _ => (1 until expInt).foldLeft(BigDecimal(0.1))((sum,_)=>sum*BigDecimal(0.1))})
        baseValue * exp
    override def tail(state: State): Option[Token] = ready(state)

    private def digit(chr:Char):Option[Int] = chr match
      case '0' => Some(0) ; case '1' => Some(1) ; case '2' => Some(2)
      case '3' => Some(3) ; case '4' => Some(4) ; case '5' => Some(5)
      case '6' => Some(6) ; case '7' => Some(7) ; case '8' => Some(8)
      case '9' => Some(9)
      case 'a' | 'A' => Some(10)
      case 'b' | 'B' => Some(11)
      case 'c' | 'C' => Some(12)
      case 'd' | 'D' => Some(13)
      case 'e' | 'E' => Some(14)
      case 'f' | 'F' => Some(15)
      case _ => None

    override def accept(state: State, char: Char): State = state match
      case State.Init => char match
        case '-' => State.DecStart(positive = false)
        case '0' => State.DecPref(positive = true)
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => 
          State.DecPart(positive = true,digits=List(digit(char).get))
        case '.' => State.FloatExpectFraction(positive = true)
        case _ => State.Err
      case State.DecStart(positive) => char match
        case '0' => State.DecPref(positive = positive)
        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => 
          State.DecPart(digits=List(digit(char).get),positive=positive)
        case '.' => State.FloatExpectFraction(positive = positive)
        case _ => State.Err
      case State.DecPref(positive) => char match
        case 'x' | 'X' => State.HexInt(positive=positive)
        case 'b' | 'B' => State.BinInt(positive=positive)
        case 'o' | 'O' => State.OctInt(positive=positive)
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' =>
          State.OctInt(positive=positive, digits=List(digit(char).get))
        case _ => 
          State.Finish( base=10, float=false, big=false, dec=List(0) )
      case State.DecPart(digits, positive) => char match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          State.DecPart(positive=positive, digits=digits ++ List(digit(char).get) )
        case 'n' =>
          State.FinishConsumed( base=10, float=false, big=true, positive=positive, dec=digits )
        case '.' =>
          State.FloatAfterPoint(dec=digits,positive=positive)
        case _ =>
          State.Finish(
            base=10, float=false, big=false,
            dec=digits,
            fraction=List(), expo=List(),
            positive=positive,
            expoPositive=false
          )
      case State.HexInt(digits, positive) => char match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' |
             'a' | 'b' | 'c' | 'd' | 'e' | 'f' |
             'A' | 'B' | 'C' | 'D' | 'E' | 'F' =>
          State.HexInt(digits :+ digit(char).get, positive=positive)
        case 'n' =>
          State.FinishConsumed( base=16, float=false, big=true, positive=positive, dec=digits )
        case _ =>
          State.Finish( base=16, float=false, big=false, positive=positive, dec=digits )
      case State.BinInt(digits, positive) => char match
        case '0' | '1' =>
          State.BinInt(digits :+ digit(char).get, positive=positive)
        case 'n' =>
          State.FinishConsumed( base=2, float=false, big=true, positive=positive, dec=digits )
        case _ =>
          State.Finish( base=2, float=false, big=false, positive=positive, dec=digits )
      case State.OctInt(digits, positive) => char match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' =>
          State.OctInt(digits :+ digit(char).get, positive=positive)
        case 'n' =>
          State.FinishConsumed( base=8, float=false, big=true, positive=positive, dec=digits )
        case _ =>
          State.Finish( base=8, float=false, big=false, positive=positive, dec=digits )
      case State.FloatAfterPoint(dec, positive) => char match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          State.FloatFraction(dec, List(digit(char).get), positive)
        case 'e' | 'E' =>
          State.ExpoSignOpt(dec,List(),positive)
        case _ => 
          State.Finish(base=10,float=true,big=false,dec=dec,List(),expo=List(),expoPositive=true,positive=positive)
      case State.FloatFraction(dec, fraction, positive) => char match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          State.FloatFraction(dec, fraction :+ digit(char).get, positive)
        case 'e' | 'E' =>
          State.ExpoSignOpt(dec, fraction, positive)
        case _ =>
          State.Finish(base=10,float=true,big=false,dec=dec,fraction=fraction,expo=List(),expoPositive=true,positive=positive)
      case State.FloatExpectFraction(dec, positive) => char match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          State.FloatFraction(dec, List(digit(char).get), positive)
        case _ =>
          State.Err
      case State.ExpoSignOpt(dec, fraction, positive) => char match
        case '+'  =>
          State.Expo(dec, fraction, List(), expoPositive=true, positive)
        case '-' =>
          State.Expo(dec, fraction, List(), expoPositive=false, positive)
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          State.Expo(dec, fraction, List(digit(char).get), expoPositive=true, positive)
        case _ =>
          State.Err
      case State.Expo(dec, fraction, expo, expoPositive, positive) => char match
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          State.Expo(dec, fraction, expo :+ digit(char).get, expoPositive, positive)
        case _ =>
          State.Finish(
            base = 10, float=true, big=true,
            dec=dec, fraction=fraction, expo=expo,
            expoPositive=expoPositive, positive=positive
          )
      case s:State.Finish => s
      case s:State.FinishConsumed => s
      case State.Err => State.Err
    
    override def end(state: State): State = state match
      case State.Init => State.Err
      case State.DecStart(positive) => State.Err
      case State.DecPref(positive) => State.Err
      case State.DecPart(digits, positive) =>                         State.Finish(base=10, float=false, big=false, dec=digits, fraction=List(), expo=List(), expoPositive=true, positive=positive)
      case State.HexInt(digits, positive)  =>                         State.Finish(base=16, float=false, big=false, dec=digits, fraction=List(), expo=List(), expoPositive=true, positive=positive)
      case State.BinInt(digits, positive)  =>                         State.Finish(base=2 , float=false, big=false, dec=digits, fraction=List(), expo=List(), expoPositive=true, positive=positive)
      case State.OctInt(digits, positive)  =>                         State.Finish(base=8 , float=false, big=false, dec=digits, fraction=List(), expo=List(), expoPositive=true, positive=positive)
      case State.FloatAfterPoint(dec, positive) =>                    State.Finish(base=10, float=true,  big=false, dec=dec,    fraction=List(), expo=List(), expoPositive=true, positive=positive)
      case State.FloatFraction(dec, fraction, positive) =>            State.Finish(base=10, float=true,  big=false, dec=dec,    fraction=fraction, expo=List(), expoPositive=true, positive=positive)
      case State.FloatExpectFraction(dec, positive) =>                State.Finish(base=10, float=true,  big=false, dec=dec,    fraction=List(), expo=List(), expoPositive=true, positive=positive)
      case State.ExpoSignOpt(dec, fraction, positive) =>              State.Finish(base=10, float=true,  big=false, dec=dec,    fraction=fraction, expo=List(), expoPositive=true, positive=positive)
      case State.Expo(dec, fraction, expo, expoPositive, positive) => State.Finish(base=10, float=true,  big=false, dec=dec,    fraction=fraction, expo=expo, expoPositive=expoPositive, positive=positive)
      case s:State.Finish => s
      case s:State.FinishConsumed => s
      case State.Err => State.Err
    
