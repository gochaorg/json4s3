package xyz.cofe.jtfm.store.json.stream

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
  enum State:
    // '-' -> DecStart
    // '0' -> DecPref
    // '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> DecPart
    // '.' -> FloatExpectFraction
    // -> Err
    case Init

    // '0' -> DecPref
    // '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> DecPart
    // '.' ->
    case DecStart

    // 'x' | 'X' -> HexInt
    // 'b' | 'B' -> BinInt
    // 'o' | 'O' -> OctInt
    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' -> OctInt
    // -> Finish
    case DecPref

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> DecPart
    // 'n' -> FinishConsumed
    // '.' -> FloatAfterPoint
    case DecPart

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    //     | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' 
    //     | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' -> HexInt
    // 'n' -> FinishConsumed
    // -> Finish
    case HexInt

    // '0' | '1' -> BinInt
    // 'n' -> FinishConsumed
    // -> Finish
    case BinInt

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' -> OctInt
    // 'n' -> FinishConsumed
    // -> Finish
    case OctInt

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> FloatFraction
    // 'e' | 'E' 
    // -> Finish
    case FloatAfterPoint

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> FloatFraction
    // 'e' | 'E'
    // -> Finish
    case FloatFraction

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> FloatFraction
    // -> Err
    case FloatExpectFraction

    // '+' | '-' -> Expo
    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->  Expo
    // -> Err
    case ExpoSignOpt

    // '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->  Expo
    // -> Finish
    case Expo

    case Finish
    case FinishConsumed
    case Err
