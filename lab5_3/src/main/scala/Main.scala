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
      println(f"a = $a%6.2f b = $b%6.2f Fa = $Fa%6.2f Fb = $Fb%6.2f x = $x%6.2f Fx = $Fx%6.2f y = $y%6.2f Fy = $Fy%6.2f")
    }
  }
}
