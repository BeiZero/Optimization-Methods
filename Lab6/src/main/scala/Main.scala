/**
  * Created by NoName on 21.02.2016.
  */

import java.util

import math._
import scala.collection.mutable.ArrayBuffer

object Main {

  def main(args: Array[String]) {

    def minMax(a: Array[Double]) : (Double, Double) = {
      if (a.isEmpty) throw new java.lang.UnsupportedOperationException("array is empty")
      a.foldLeft((a(0), a(0)))
      { case ((min, max), e) => (math.min(min, e), math.max(max, e))}
    }

    var d = new ArrayBuffer[Double]()
    d += args(0).toDouble
    d += args(1).toDouble
    d += args(2).toDouble
    val (a,b) = (args(3).toDouble,args(4).toDouble)
    val (alpha, eps) = (args(5).toDouble,args(6).toDouble)

    def phi(x: Double): Double = {
      var result: Double = 0
      if (0 <= x && x <= 2*d(0)) result = abs(x-d(0))-d(0)
      if (2*d(0) <= x && x <= 2*d(0) + 2*d(1)) result = abs(x-2*d(0)-d(1))-d(1)
      if (2*d(0) + 2*d(1) <= x) result = abs(x-2*d(0)-2*d(1)-d(2))-d(2)

      return result
    }

    var x = new ArrayBuffer[Double]()
    x += a
    x += b
    var fx = new ArrayBuffer[Double]()
    fx += phi(x(0))
    fx += phi(x(1))


    var (w,xtick) = if (fx(0) <= fx(1)) (fx(0),x(0)) else (fx(1), x(1))
    var u: ArrayBuffer[Double] = new ArrayBuffer[Double]()
    var xi: ArrayBuffer[Double] = new ArrayBuffer[Double]()

    var k = 0
    u += (1/2.0) * (alpha*(x(0)-x(1)) + (fx(0) + fx(1)))
    xi += (x(0)+x(1))/2 + (fx(0)-fx(1))/(2*alpha)
    var nu = u(k)
    k += 1
    while ((w - nu) < eps) {
      var p = u.indexOf(nu)
      var xtmp = xi(p)
      var fxtmp = phi(xtmp)
      if (fxtmp > 2*w - nu) {
        xi.remove(p)
        u.remove(p)
        nu = minMax(u.toArray)._1
      }
      else {
        if (fxtmp < w) {
          w = fxtmp
          xtick = xtmp
          for (i <- 0 to u.length) {
            if (u(i) >= w) {
              u.remove(i)
              xi.remove(i)
            }
          }
        }
        u(k) = (fxtmp + nu)/2
        xi(k) = xi(p) + (fxtmp - nu)/(2*alpha)
        xi(p) = 2*xi(p) - xi(k)
        u(p) = u(k)
        k += 1
        nu = minMax(u.toArray)._1
      }

    }

    println(f"W = $w%10.5f, x' = $xtick%10.5f")
  }
}
