package lab2

import scala.math._

/**
  * Created by NoName on 18.02.2016.
  */
object Main {

  def main(args: Array[String]): Unit = {
    println("Общий вид уравнения phi(x)=(x-d)(x-l)^3")

    def phi(x:Double, d: Double, l: Double): Double = (x - d) * pow((x - l),3)

    val eps = 0.005

    def Fib(k: Int): Int = k match {
      case 0 => 0
      case 1 => 1
      case n => Fib(n-1) + Fib(n-2)
    }

    val lambda_fib:Double = Fib(5)/Fib(6).toDouble
    val lambda_gold = (sqrt(5)-1)/2

    def lambda(g: Boolean) = if (g) lambda_gold else lambda_fib

    val data = Method(phi, args(0).toDouble, args(1).toDouble, lambda(true), eps)

    val tableHeader = Array("k","a","b","d0","d1","d2","x","Fx","y","Fy")
    tableHeader.foreach(s => printf("%1$10s", s))
    println
    var k = 1
    for (row <- data) {
      printf("%1$10d", k)
      row.foreach(f => printf("%1$ 10.5f", f))
      println
      k += 1
    }
  }
}
