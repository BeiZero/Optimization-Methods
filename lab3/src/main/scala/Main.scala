<<<<<<< HEAD
import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    val d = args(0).toDouble
    var a = args(1).toDouble
    var b = args(2).toDouble
    var lambda = args(3).toDouble
    val eps = args(4).toDouble
    var delta = lambda*(b - a)
    var x = b - delta
    delta *= lambda
    var y = x + delta
    def phi(x:Double) = abs(x-d)
    while(b-a>eps){
      delta*=lambda
      if(phi(x)<=phi(y)){
        b = y
        y = x
        x = y - delta
      } else {
        a = x
        x = y
        y = x + delta
      }
    }
    println(f"phi($x%2.2f)=${phi(x)}%2.2f")
    println(f"phi($y%2.2f)=${phi(y)}%2.2f")
=======
/**
  * Created by NoName on 21.02.2016.
  */

import math._

object Main {

  def phi(x: Double, d: Double) = abs(x-d)

  def main(args: Array[String]) {
    val d = args(0).toDouble
    val a = args(1).toDouble
    val b = args(2).toDouble
    val lambda = args(3). toDouble
    val eps = args(4).toDouble
    println(f"Функция: y = |x - $d|")
    println(f"Входные данные: a = $a, b = $b, lambda = $lambda, eps = $eps")
    val (x, fx) = Method(phi(_, d), a0 = a, b0 = b, lambda = lambda, eps = eps)
    println(f"Ответ: ($x%9.5f, $fx%9.5f) с точностью $eps%9.5f")
>>>>>>> pr/7
  }
}
