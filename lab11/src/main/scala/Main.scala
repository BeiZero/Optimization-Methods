import scala.math._

object Main extends App {
  val (a,b1,b2,d1,d2) = (args(0).toDouble,args(1).toDouble,args(2).toDouble,args(3).toDouble,args(4).toDouble)
  def sqr(x:Double) = pow(x,2)
  def phi(x:Array[Double]) = a+b1*x(0)+b2*x(1)+(sqr(x(0))*d1+2.0*x(0)*x(1)+sqr(x(1))*d2)/2.0
  def prfi1x1(x:Array[Double]) = b1+x(0)*d1+x(1)
  def prfi1x2(x:Array[Double]) = b2+x(0)+x(1)*d2
  var iter = 0
  val x = Array[Double](0,0)
  val s = Array[Double](prfi1x1(x),prfi1x2(x))
  println(s"iter = $iter x = (${x(0)},${x(1)}) prfi1(x) = ${prfi1x1(x)} prfi2(x) = ${prfi1x2(x)} phi(x) = ${phi(x)}")
  iter += 1
  for(i <- 0 to 1){
    val lambda = -(prfi1x1(x)*s(0)+prfi1x2(x)*s(1))/(d1*sqr(s(0))+d2*sqr(s(1)))
    x(0)=x(0)+lambda*s(0)
    x(1)=x(1)+lambda*s(1)
    s(0)=prfi1x1(x)
    s(1)=prfi1x2(x)
    iter += 1
  }
  println("Min")
  println(s"x =  (${x(0)},${x(1)})")
  println(s"f(x) = ${phi(x)}")
  println(s"prfi1(x) = ${prfi1x1(x)} prfi2(x) = ${prfi1x2(x)}")
}
