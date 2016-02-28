/**
  * Created by NoName on 21.02.2016.
  */

import math._
import scala.collection.mutable.ArrayBuffer

object Main {

  trait IndexesOf[T] extends ArrayBuffer[T] {
    def indexesOf(n: T): ArrayBuffer[Int] = {
      var result = ArrayBuffer.empty[Int]
      for (i <- 0 until this.length) if (this(i) == n) result += i
      return result
    }
  }

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
    val (alpha, eps) = (args(3).toDouble,args(4).toDouble)
    val (a,b): (Double, Double) = if (args.length == 5) (0,2*d(0)+2*d(1)+2*d(2)) else (args(5).toDouble, args(6).toDouble)

    def phi(x: Double): Double = {
      var result: Double = 0
      if (0 <= x && x <= 2*d(0)) result = abs(x-d(0))-d(0)
      if (2*d(0) <= x && x <= 2*d(0) + 2*d(1)) result = abs(x-2*d(0)-d(1))-d(1)
      if (2*d(0) + 2*d(1) <= x && x<=2*d(0) + 4*d(1)) result = abs(x-2*d(0)-2*d(1)-d(2))-d(2)

      return result
    }

    var x = new ArrayBuffer[Double]()
    x += a
    x += b
    var fx = new ArrayBuffer[Double]()
    fx += phi(x(0))
    fx += phi(x(1))

    var w = new ArrayBuffer[Double]()
    var xtick = new ArrayBuffer[Double]()
    xtick += (if (fx(0) <= fx(1)) x(0) else x(1))
    w += (if (fx(0) <= fx(1)) fx(0) else fx(1))

    var u = new ArrayBuffer[Double] with IndexesOf[Double]
    var xi = new ArrayBuffer[Double]()
    u += (1/2.0) * (alpha*(x(0)-x(1)) + (fx(0) + fx(1)))
    xi += (x(0) + x(1))/2 + (fx(0) - fx(1))/(2*alpha)
    var nu = u(0)

    while ((w.last - nu) >= eps) {
      var ps = u.indexesOf(nu)
      var p = u.indexOf(nu)
      var xtmp = xi(p)
      var fxtmp = phi(xtmp)

      if (fxtmp > 2 * w.last - nu) {
        xi.remove(p)
        u.remove(p)
        nu = minMax(u.toArray)._1
      }
      else {
        if (fxtmp <= w.last) {
          w += fxtmp
          xtick += xtmp
          println(f"x' = ${xtick.last}, W = ${w.last}")
          for (ui <- u) {
            if (ui >= w.last) {
              val k = u.indexOf(ui)
              if (k >= 0) {
                u.remove(k)
                xi.remove(k)
              }
            }
          }
        }
        u += (fxtmp + nu) / 2
        xi += xtmp + (fxtmp - nu) / (2 * alpha)
        if (p >= 0 && p < u.length) {
          xi(p) = 2 * xtmp - xi.last
          u(p) = u.last
        }
        nu = minMax(u.toArray)._1
      }
    }
  }
}
