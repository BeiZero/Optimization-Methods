import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    println("Общий вид уравнения phi(x)=(x-d)((x-l)^3)")
    var x = 0
    var dx = 1
    val m = 2
    def phi(x:Double) = (x - args(0).toDouble)*pow((x-args(1).toDouble),3)
    var y = x + dx
    if(phi(y)>phi(x)){
      val z = x
      x = y
      y = z
      dx = -dx
    }
    while(phi(y)<=phi(x)){
      x = y
      dx = m*dx
      y = x + dx
    }
    val(a,b) = if(dx>0) (x-dx/m,y) else (y,x-dx/m)
    println(f"a = $a%2.2f")
    println(f"b = $b%2.2f")
    println(f"Fx = ${phi(x)}%2.2f")
    println(f"Fy = ${phi(y)}%2.2f")
    println(f"x = $x%2.2f")
    println(f"y = $y%2.2f")
  }
}
