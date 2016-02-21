import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    val (d,l) = (args(0).toDouble,args(1).toDouble)
    val a = args(2).toDouble
    val b = args(3).toDouble
    var y = args(4).toDouble
    val eps = 0.0001
    def phi(x:Double) = (x-d)*pow((x-l),3)
    def pphi(x:Double) = (-3*d-l+4*x)*pow((-l+x),2)
    def ppphi(x:Double) = 6*(d+l-2*x)*(l-x)
    var x = 0.0
    def M:Unit = {
      x = y - pphi(y)/ppphi(y)
      if(x<a||x>b) return
      if(abs(phi(x)-phi(y))>eps){
        y = x
        M
      }
    }
    M
    println(f"y = $y%6.4f Fy = ${phi(y)}%6.4f")
  }
}
