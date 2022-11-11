package xyz.cofe.jtfm.store.json.stream

class ShowTest extends munit.FunSuite:
  trait Show[T]:
    def show(t:T):String
  
  object Show:
    given Show[Int] with
      def show(i:Int) = s"$i"

    given Show[EmptyTuple] with
      def show(e:EmptyTuple)=""

    given [H:Show,T<:Tuple:Show]:Show[H *: T] with
      def show(t: H *: T):String = 
        summon[Show[H]].show(t.head) + ", " + summon[Show[T]].show(t.tail)

  extension [T:Show](t:T)
    def show:String = summon[Show[T]].show(t)

  test("show test") {
    println("===================")
    println("=== show test  ====")
    println( (5, 6, 1, 2, 3).show )
  }
