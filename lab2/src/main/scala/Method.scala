package lab2

/**
  * Created by NoName on 18.02.2016.
  */

import scala.collection.mutable.ArrayBuffer

object Method {

  def apply(f: (Double, Double, Double) => Double,
            d:Double,
            l:Double,
            lambda: Double,
            eps: Double): ArrayBuffer[Array[Double]] = {
    println(f"лямбда = $lambda%10.5f")
    println(f"эпсилон = $eps%10.5f")
    val lab1result = lab1.Method.run(f,d,l)
    var (a,b) = (lab1result._1, lab1result._2)

    var result: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]()
    def phi_c(x:Double) = f(x, d, l)

    def method: Unit = {
      var d0 = b - a
      var d1 = lambda * d0
      var x = b - d1
      var y = a + d1
      var Fx = phi_c(x)
      var Fy = phi_c(y)
      var d2 = d0 - d1

      while (d2 < d1 && d1 > eps) {
        result += Array(a, b, d0, d1, d2, x, Fx, y, Fy)
        if (Fx <= Fy) {
          b = y
          y = x
          Fy = Fx
          x = b - d2
          Fx = phi_c(x)
        }
        else {
          a = x
          x = y
          Fx = Fy
          y = a + d2
          Fy = phi_c(y)
        }
        d0 = d1
        d1 = d2
        d2 = d0 - d1
      }
      if (d2 >= d1 && d1 > eps) method
    }

    method

    return result
  }
}
