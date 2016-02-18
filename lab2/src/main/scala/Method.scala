package lab2

/**
  * Created by NoName on 18.02.2016.
  */
import lab1.Method

import scala.collection.mutable.ArrayBuffer


object Method {

  def curry(f: (Double, Double, Double) => Double, d: Double, l: Double): (Double) => Double = {
    f(_, d,l)
  }



  def run(f: (Double, Double, Double) => Double, d:Double, l:Double, lambda: Double, eps: Double): ArrayBuffer[ArrayBuffer[Double]] = {
    val lab1result = lab1.Method.run(f,d,l)
    var (a,b) = (lab1result._1, lab1result._2)

    var result: ArrayBuffer[ArrayBuffer[Double]] = ArrayBuffer[ArrayBuffer[Double]]()

    def method(m: Double, n: Double): Unit = {
      var d0 = n-m

      var d1 = lambda * d0
      var x = n-d1
      var y = m+d1

      val phi_c = curry(f, d, l)

      var Fx = phi_c(x)
      var Fy = phi_c(y)

      var d2 = d0-d1

      var stepresult = new ArrayBuffer[Double]()
      while (d2<d1 && d1>eps) {
        stepresult.clear()
        stepresult += a
        stepresult += b
        stepresult += d0
        stepresult += d1
        stepresult += d2
        stepresult += x
        stepresult += Fx
        stepresult += y
        stepresult += Fy
        result += stepresult
        if (Fx<=Fy) {
          b=y
          y=x
          Fy=Fx
          x=n-d2
          Fx=phi_c(x)
        }
        else {
          a=x
          x=y
          Fx=Fy
          y=m+d2
          Fy=phi_c(y)
        }
        d0=d1
        d1=d2
        d2=d0-d1
      }

      if (d2>=d1) method(a,b)

      stepresult.clear()
      stepresult += a
      stepresult += b
      stepresult += d0
      stepresult += d1
      stepresult += d2
      stepresult += x
      stepresult += Fx
      stepresult += y
      stepresult += Fy
      result += stepresult

      if (Fy<Fx) {
        x=y
        Fx=Fy
      }
    }

    result
  }
}
