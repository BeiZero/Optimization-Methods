import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    val (d,l) = (args(0).toDouble,args(1).toDouble)
    def phi(x:Double) = (x-d)*pow((x-l),3)
    var (a,b,y,eps) = (args(2).toDouble,args(3).toDouble,args(4).toDouble,args(5).toDouble)
    var Fa = phi(a)
    var Fb = phi(b)
    var Fy = phi(y)
    var x = 0.5*(((b*b-y*y)*Fa-(b*b-a*a)*Fy+(y*y-a*a)*Fb)/((b-y)*Fa-(b-a)*Fy+(y-a)*Fb))
    var Fx = phi(x)
    while (abs(x-y)>eps){
      if(x>y){
        val z = y
        val Fz = Fy
        y = x
        Fy = Fx
        x = z
        Fx = Fz
      }
      if(Fx<=Fy){
        b = y
        Fb = phi(b)
        y = x
        Fx = phi(y)
      } else {
        a = x
        Fa = phi(a)
      }
      x = 0.5*(((b*b-y*y)*Fa-(b*b-a*a)*Fy+(y*y-a*a)*Fb)/((b-y)*Fa-(b-a)*Fy+(y-a)*Fb))
      Fx = phi(x)
      println(f"a = $a%9.5f b = $b%9.5f Fa = $Fa%9.5f Fb = $Fb%9.5f x = $x%9.5f Fx = $Fx%9.5f y = $y%9.5f Fy = $Fy%9.5f")
    }
    println("Ответ: ")
    if(phi(y)>phi(x)) println(f"x = $x%9.5f Fx = ${phi(x)}%9.5f")
    else println(f"y = $y%9.5f Fy = ${phi(y)}%9.5f")
  }
}
