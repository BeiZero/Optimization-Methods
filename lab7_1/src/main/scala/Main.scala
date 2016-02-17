import scala.math._

object Main {

  def main(args: Array[String]) {
    val (d1,d2,d3) = (args(0).toDouble,args(1).toDouble,args(2).toDouble)
    def phi(x:Array[Double]) = pow(x(0) - d1,2)/4+pow(x(1)-d2,2)/9 + d3
    val (alpfa,beta,eps) = (2,-0.5,0.05)
    var iter = 0
    val lambda = Array[Double](2,2)
    var x0 = Array[Double](0,0)
    val xz = x0
    val xs = xz
    def M:Unit = {
      show()
      iter += 1
      for(i <- 0 until  2){
        xs(i) = xz(i) + lambda(i)
        if(phi(xs)<phi(xz)){
          xz(i) = xs(i)
          lambda(i) = lambda(i)*alpfa
        } else {
          lambda(i) = lambda(i)*beta
        }
      }
      if(abs(x0(0)-xz(0))>eps||abs(x0(1)-xs(1))>eps||abs(lambda(0))>eps||abs(lambda(1))>eps){
        x0 = xz
        M
      }
    }
    M
    def show() = println(f"iter = $iter xz = (${xz(0)}%2.2f,${xz(1)}%2.2f) lambda = (${lambda(0)}%2.2f,${lambda(1)}%2.2f) phi(x*) = ${phi(xz)}%2.2f")
    show()
  }
}
