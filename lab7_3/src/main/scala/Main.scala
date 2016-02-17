import scala.math._

object Main {

  def main(args: Array[String]) {
    val (d1,d2,d3) = (args(0).toDouble,args(1).toDouble,args(2).toDouble)
    def phi(x:Array[Double]) = pow(x(0) - d1,2)/4+pow(x(1)-d2,2)/9 + d3
    val (alpfa,beta,eps) = (args(3).toDouble,args(4).toDouble,args(5).toDouble)
    def argmin(x:Array[Double],l:Double,n:Int) ={
      var lambda = l
      val xs = x(n)
      val xnew = x
      xnew(n) = xnew(n) + lambda
      while(abs(x(n)-xnew(n))>eps){
        while(phi(xnew)<phi(x)){
          x(n) = xnew(n)
          lambda = lambda*alpfa
          xnew(n) = x(n) + lambda
        }
        lambda=lambda * beta
        xnew(n) = x(n)+lambda
      }
      x(n) - xs
    }
    var iter = 0
    val lambda = Array[Double](2,2)
    var x0 = Array[Double](0,0)
    val xz = x0
    def M:Unit = {
      println(s"iter = $iter xz = (${xz(0)},${xz(1)}) lambda = (${lambda(0)},${lambda(1)}) phi(x*) = ${phi(xz)}")
      iter += 1
      val xs = xz
      for(i <- 0 until  2){
        lambda(i) = argmin(xz,lambda(i),i)
        xs(i) = xz(i) + lambda(i)
        xz(i) = xs(i)
      }
      if(abs(x0(0)-xz(0))>eps||abs(x0(1)-xs(1))>eps){
        x0 = xz
        M
      }
    }
    M
    println(s"iter = $iter xz = (${xz(0)},${xz(1)}) lambda = (${lambda(0)},${lambda(1)}) phi(x*) = ${phi(xz)}")
    println(s"phi(x) = ${phi(xz)}")
  }
}
