import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    val d = args(0).toDouble
    var a = args(1).toDouble
    var b = args(2).toDouble
    var lambda = args(3).toDouble
    val eps = args(4).toDouble
    var delta = lambda*(b - a)
    var x = b - delta
    delta *= lambda
    var y = x + delta
    def phi(x:Double) = abs(x-d)
    while(b-a>eps){
      delta*=lambda
      if(phi(x)<=phi(y)){
        b = y
        y = x
        x = y - delta
      } else {
        a = x
        x = y
        y = x + delta
      }
    }
    println(f"phi($x%2.2f)=${phi(x)}%2.2f")
    println(f"phi($y%2.2f)=${phi(y)}%2.2f")
  }
}
