import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    val (d,l) = (args(0).toDouble,args(1).toDouble)
    def phi(x:Double) = (x-d)*pow((x-l),3)
    var (a,b,y,eps) = (args(2).toDouble,args(3).toDouble,args(4).toDouble,args(5).toDouble)
    var Fa = phi(a)
    var Fb = phi(b)
    var Fy = phi(y)
    var x = y - (b-y)*(y-a)*(Fb-Fa)/((b-y)*Fa-(b-a)*Fy+(y-a)*Fb)
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
      x = y - (b-y)*(y-a)*(Fb-Fa)/((b-y)*Fa-(b-a)*Fy+(y-a)*Fb)
      Fx = phi(x)
      println(f"a = $a%10.2f b = $b%10.2f Fa = $Fa%10.2f Fb = $Fb%10.2f x = $x%10.2f Fx = $Fx%10.2f y = $y%10.2f Fy = $y%10.2f")
    }
    println(f"a = $a%10.2f b = $b%10.2f Fa = $Fa%10.2f Fb = $Fb%10.2f x = $x%10.2f Fx = $Fx%10.2f y = $y%10.2f Fy = $y%10.2f")
  }
}
