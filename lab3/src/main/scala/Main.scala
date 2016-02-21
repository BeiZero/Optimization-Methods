/**
  * Created by NoName on 21.02.2016.
  */

import math._

object Main {

  def phi(x: Double, d: Double, l: Double) = (x-d)*pow((x-l),3)

  def main(args: Array[String]) {
    val d = args(0).toDouble
    val l = args(1).toDouble
    val a = args(2).toDouble
    val b = args(3).toDouble
    val lambda = args(4). toDouble
    val eps = args(5).toDouble
    println(f"Функция: y = (x - $d)(x - $l)^3")
    println(f"Входные данные: a = $a, b = $b, lambda = $lambda, eps = $eps")
    val (x, fx) = Method(phi(_, d, l), a0 = a, b0 = b, lambda = lambda, eps = eps)
    println(f"Ответ: ($x%9.5f, $fx%9.5f) с точностью $eps%9.5f")
  }
}
