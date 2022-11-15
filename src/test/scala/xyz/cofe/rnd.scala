package xyz.cofe

import java.util.concurrent.ThreadLocalRandom

object rnd {
  private val r = ThreadLocalRandom.current()

  def int:Int = r.nextInt()
  def double:Double = r.nextDouble()
  def int(limit:Int) = r.nextInt(limit)
  def int(min:Int,max:Int) = r.nextInt( Math.abs(max-min) ) + Math.min(max,min)
  def bigInt:BigInt = BigInt( int ) * BigInt( int )

  val lowLetters = "qwertyuiopasdfghjklzxcvbnm"
  lazy val hiLetters = lowLetters.toUpperCase()
  lazy val letters = hiLetters ++ lowLetters

  def string(minLen:Int,maxLen:Int) = (0 until int(minLen,maxLen)).map { _ =>
    letters.charAt(int(letters.length))
  }.mkString
}
