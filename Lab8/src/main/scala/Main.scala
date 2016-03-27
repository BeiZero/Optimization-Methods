// import scalaz._
// import Scalaz._
// import scalaz.effect._
import java.util
import scala.math._
import scala.language.postfixOps

object Main extends App {
  val n = 2
  val Array(d1, d2, d3) = args map (_ toInt)
  def phi(x:Array[Double]) = pow(x(0) - d1, 2) / 4 + pow(x(1) - d2, 2) / 9 + d3
  var x = Array(Array(0.0, 0.0), Array(1.0, 0.0), Array(0.0, 1.0),Array(0.0,0.0),Array(0.0,0.0),Array(0.0,0.0),Array(0.0,0.0))
  val (alpha, beta, gamma, delta, eps) = (1, 0.5, 2, 0.5, 0.001)
  var s = 0.0
  do {
    var h = 0
    var l = 0
    var g = 0
    var max = phi(x(0))
    var min = phi(x(0))
    for(i <- 1 to n){
      if(phi(x(i))<min){
        min = phi(x(i))
        l = i
      }
      if(phi(x(i))>max){
        max = phi(x(i))
        h = i
      }
    }
    max = phi(x(0))
    for(i <- 1 to n)
      if(phi(x(i))>max&&i!=h){
        max = phi(x(i))
        g = i
      }
    x(n+1)(0) = (x(0)(0) + x(1)(0) + x(2)(0) - x(h)(0)) / n
    x(n+1)(1) = (x(0)(1) + x(1)(1) + x(2)(1) - x(h)(1)) / n

    for(i <- 0 until n) x(n + 2)(i) = 2 * x(n + 1)(i) - x(h)(i)

    if(phi(x(n+2))<phi(x(l))){
      x(n + 3)(0) = x(n + 1)(0) + alpha * (x(n + 2)(0) - x(n + 1)(0));
      x(n + 3)(1) = x(n + 1)(1) + alpha * (x(n + 2)(1) - x(n + 1)(1));
      if(phi(x(n+3))<phi(x(l)))
        x(h) = x(n+3)
      else
        x(h) = x(n+2)
    } else {
      if(phi(x(n+2))>phi(x(g))){
        if(phi(x(n+2))<=phi(x(h))) x(h) = x(n+2)
        x(n + 4)(0) = x(n + 1)(0) + beta * (x(h)(0) - x(n + 1)(0));
        x(n + 4)(1) = x(n + 1)(1) + beta * (x(h)(1) - x(n + 1)(1));
        if(phi(x(n+4))<=phi(x(h))) x(h) = x(n+4)
        else for(i <- 0 until n){
          x(i)(0) = x(i)(0) + beta * (x(i)(0) - x(l)(0));
          x(i)(1) = x(i)(1) + beta * (x(i)(1) - x(l)(1));
        }
      } else x(h) = x(n+2)
      min = phi(x(0))
      for(i <- 1 to n)
        if(phi(x(i))<min){
          min = phi(x(i))
          l = i
        }
    }
    s = 0
    for(i <- 0 to n){
      s += pow(phi(x(i)) - phi(x(n+1)),2)
    }
    s /= (n + 1)
    for(i <- 0 until n){
      x(i)(0) = x(l)(0) + delta*(x(i)(0)-x(l)(0))
      x(i)(1) = x(l)(1) + delta*(x(i)(1)-x(l)(1))
    }
    show()
  } while (sqrt(s) > eps)
  def show() = {
    for(i <- 0 until 6) print(f"phi(${x(i)(0)}%2.2f,${x(i)(1)}%2.2f) = ${phi(x(i))}%2.2f  ")
    println
  }
  println(f"Ответ: phi(${x(1)(0)}%2.2f,${x(1)(1)}%2.2f) = ${phi(x(1))}%2.2f")
}
