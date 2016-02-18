package lab2

import scala.math._

/**
  * Created by NoName on 18.02.2016.
  */
object Main {

  def main(args: Array[String]): Unit = {
    println("Общий вид уравнения phi(x)=(x-d)(x-l)")

    def phi(x:Double, d: Double, l: Double): Double = (x - d) * (x - l)

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


    val data = Method.run(phi, args(0).toDouble, args(1).toDouble, lambda, eps)


    println(f"k\ta\tb\td0\td1\td2\tx\tFx\ty\tFy")
    var k = 1
    for (row <- data) {
      println(f"$k%2d\t${row(0)}%2.2f\t${row(1)}%2.2f\t${row(2)}%2.2f\t${row(3)}%2.2f\t${row(4)}%2.2f\t${row(5)}%2.2f\t${row(6)}%2.2f\t${row(7)}%2.2f\t${row(8)}%2.2f\t")
      k += 1
    }
  }
}
