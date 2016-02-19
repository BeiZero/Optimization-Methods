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



  def run(f: (Double, Double, Double) => Double, d:Double, l:Double, lambda: Double, eps: Double): ArrayBuffer[Tuple9[Double,Double,Double,Double,Double,Double,Double,Double,Double]] = {
    val lab1result = lab1.Method.run(f,d,l)
    var (a,b) = (lab1result._1, lab1result._2)

    var result: ArrayBuffer[Tuple9[Double,Double,Double,Double,Double,Double,Double,Double,Double]] = new ArrayBuffer[Tuple9[Double,Double,Double,Double,Double,Double,Double,Double,Double]]()
    val phi_c = curry(f, d, l)

    def method(m: Double, n: Double): Unit = {
      var d0 = n-m

      var d1 = lambda * d0
      var x = n-d1
      var y = m+d1

      var Fx = phi_c(x)
      var Fy = phi_c(y)

      var d2 = d0-d1


      while (d2<d1 && d1>eps) {
        result += new Tuple9(a,b,d0,d1,d2,x,Fx,y,Fy)
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

      //if (d2>=d1) method(a,b)



      if (Fy<Fx) {
        x=y
        Fx=Fy
      }
      result += new Tuple9(a,b,d0,d1,d2,x,Fx,y,Fy)
    }

    method(a,b)

    result
  }
}
