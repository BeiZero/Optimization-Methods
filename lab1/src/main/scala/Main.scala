package lab1

import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    println("Общий вид уравнения phi(x)=(x-d)(x-l)^3")
    def phi(x:Double, d: Double, l: Double): Double = (x - d)*pow((x-l),3)
    val result: (Double, Double, Double, Double, Double, Double) = Method.run(phi, args(0).toDouble, args(1).toDouble)
    println(f"a = ${result._1}%2.2f")
    println(f"b = ${result._2}%2.2f")
    println(f"Fx = ${{result._3}}%2.2f")
    println(f"Fy = ${{result._4}}%2.2f")
    println(f"x = ${result._5}%2.2f")
    println(f"y = ${result._6}%2.2f")
  }
}
