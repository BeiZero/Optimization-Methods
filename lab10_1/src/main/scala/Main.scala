import scala.math._

object Main extends App{
  val (d1,d2,d3,eps) = (args(0).toDouble,args(1).toDouble,args(2).toDouble,args(3).toDouble)
  def sqr(x:Double) = pow(x,2)
  def phi(x:Array[Double]) = (1+exp(-x(0)-x(1)))*(sqr(x(0)-d1)/2.0+sqr(x(1)-d2)/9.0)+d3
  def prfi1x1(x:Array[Double]) = (x(0)-d1)*(exp(-x(0)-x(1))+1)-exp(-x(0)-x(1))*(sqr(x(0)-d1)/2.0+sqr(x(1)-d2)/9.0)
  def prfi2x1(x:Array[Double]) = exp(-x(0)-x(1))*(9*d1*d1-18.0*d1*(x(0)-2.0)+2.0*d2*d2-4.0*d2*x(1)+9.0*x(0)*x(0)+
    18.0*exp(x(0)+x(1))-36.0*x(0)+2.0*x(0)*x(0)+18)/18.0
  def prfi1x2(x:Array[Double]) = 2.0*(x(1)-d2)/9.0*(exp(-x(0)-x(1))+1)-exp(-x(0)-x(1))*(sqr(x(0)-d1)/2.0+sqr(x(1)-d2)/9.0)
  def prfi2x2(x:Array[Double]) = exp(-x(0)-x(1))*(9*d1*d1-18.0*x(0)*d1+2.0*d2*d2-4*d2*(x(1)-2)+2.0*x(1)*x(1)+
    4.0*exp(x(0)+x(1))-8.0*x(1)+9.0*x(0)*x(0)+4)/18.0
  def prfi1(x:Array[Double]) = prfi1x1(x)+prfi1x2(x)
  def prfi2(x:Array[Double]) = prfi2x1(x)+prfi2x2(x)
  var iter = 0
  val x = Array[Double](0,0)
  while (abs(prfi1(x))>eps){
    show()
    iter += 1
    val p1 = prfi1x2(x)
    val p2 = prfi2x2(x)
    x(0) = x(0)-prfi1x1(x)/prfi2x1(x)
    x(1) = x(1)-p1/p2;
  }
  def show() = println(f"iter = $iter x = (${x(0)}%2.2f,${x(1)}%2.2f) prfi1(x) = ${prfi1(x)}%2.2f prfi2(x) = ${prfi2(x)}%2.2f phi(x) = ${phi(x)}%2.2f")
  println("Min")
  println(f"x = (${x(0)}%2.2f,${x(1)}%2.2f)")
  println(f"f(x) = ${phi(x)}%2.2f")
}
