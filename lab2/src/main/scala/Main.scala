package lab2

import scala.math._

/**
  * Created by NoName on 18.02.2016.
  */
object Main {

  def main(args: Array[String]): Unit = {
    println("Общий вид уравнения phi(x)=(x-d)(x-l)")

    def phi(x:Double, d: Double, l: Double): Double = (x - d) * pow((x - l),3)

    val eps = 0.05

    def Fib(k: Int): Int = {
      if (k == 0) {
        0
      }
      else {
        if (k == 1) {
          1
        }
        else Fib(k-2) + Fib(k-1)
      }
    }

    val lambda = Fib(5)/Fib(6)
    val lambda_gold = (sqrt(5)-1)/2


    val data = Method.run(phi, args(0).toDouble, args(1).toDouble, lambda_gold, eps)


    println(f" k\ta\t\tb\t\td0\t\td1\t\td2\t\tx\t\tFx\t\ty\t\tFy")
    var k = 1
    for (row <- data) {
      println(f"$k%2d\t${row._1}%2.2f\t${row._2}%2.2f\t${row._3}%2.2f\t${row._4}%2.2f\t${row._5}%2.2f\t${row._6}%2.2f\t${row._7}%2.2f\t${row._8}%2.2f\t${row._9}%2.2f\t")
      k += 1
    }
  }
}
