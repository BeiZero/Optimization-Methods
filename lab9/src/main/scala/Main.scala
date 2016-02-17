import scala.math._

object Main extends App{
  val (d1,d2,d3,eps) = (args(0).toDouble,args(1).toDouble,args(2).toDouble,args(3).toDouble)
  def sqr(x:Double) = pow(x,2)
  def phi(x:Array[Double]) = sqr(x(0)-d1)/4+sqr(x(1)-d2)/9 + d3
  def prphi(x:Array[Double]) = (x(0)-d1)/2.0+2.0* (x(1)-d2)/9.0
  def prfix1(x:Array[Double]) = (x(0)-d1)/2.0
  def prfix2(x:Array[Double]) = 2.0*(x(1)-d2)/9.0
  def argmin(x:Array[Double],s:Array[Double]) = (-s(0)*(x(0)-d1)/2.0-2.0*s(1)*(x(1)-d2)/9.0)/(sqr(s(0))/2.0+2.0*sqr(s(1))/9.0)
  var iter = 0
  val x = Array[Double](0,0)
  val s = Array[Double](0,0)
  var lambda = 0.0
  while(abs(prphi(x))>eps){
    s(0) = -prfix1(x)
    s(1) = -prfix2(x)
    lambda = argmin(x,s)
    x(0)=x(0)+lambda*s(0)
    x(1)=x(1)+lambda*s(1)
    println(s"iter = $iter x = (${x(0)},${x(1)}) lambda = $lambda prfix1(x) = ${prfix1(x)} prfix2(x) = ${prfix2(x)} phi(x) = ${phi(x)}")
    iter += 1
  }
  println("Min")
  println(s"x1 = ${x(0)}")
  println(s"x2 = ${x(1)}")
  println(s"f(x) = ${phi(x)}")
}
