import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    val (d,l) = (args(0).toDouble,args(1).toDouble)
    val a = args(2).toDouble
    val b = args(3).toDouble
    var y = args(4).toDouble
    val eps = 0.025
    def phi(x:Double) = (x-d)*pow((x-l),3)
    def pphi(x:Double) = (phi(b)-phi(a))/(b-a)
    def ppphi(y:Double) = ((b-y)*phi(a)-(b-a)*phi(y)+(y-a)*phi(b))/(y-a)/(b-y)/(b-a)
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
    println(f"y = $y%6.2f Fy = ${phi(y)}%6.2f")
  }
}
