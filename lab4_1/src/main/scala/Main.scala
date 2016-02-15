import scala.math._

object Main {
  def main(args: Array[String]) {
    val d = args(0).toDouble
    val l = args(1).toDouble
    var a = args(2).toDouble
    var b = args(3).toDouble
    val eps = args(4).toDouble
    def phi(x:Double) = (x-d)*pow((x-l),3)
    def pphi(x:Double) = pow(x-l,2)*(4*x-3*d-l)
    var y = a
    if(pphi(y)>0) {M;return}
    y = b
    if(pphi(y)<0) {M;return}
    while(b-a>eps){
      y = (a+b)/2
      val PRF = pphi(y)
      if(PRF == 0) {M;return}
      if(PRF<0) a = y
      if(PRF>0) b = y
      M
    }
    def M = println(f"a = $a%6.2f b = $b%6.2f y = $y%6.2f Fy = ${phi(y)}%6.2f PRF = ${pphi(y)}%6.2f")
  }
}
