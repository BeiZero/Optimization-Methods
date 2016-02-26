import scala.math._

object Main {
  def main(args: Array[String]): Unit = {
    val (d,l) = (args(0).toDouble,args(1).toDouble)
    def phi(x:Double) = (x-d)*pow((x-l),3)
    var (a,b,y,eps) = (args(2).toDouble,args(3).toDouble,args(4).toDouble,args(5).toDouble)
    var Fa = phi(a)
    var Fb = phi(b)
    var Fy = phi(y)
    var x = y - (-3*d-l+4*y)*pow((-l+y),2)/(6*(d+l-2*y)*(l-y))
    var Fx = phi(x)
    while (abs(x-y)>eps&&a<=x&&x<=b){
      println(f"a = $a%9.5f b = $b%9.5f Fa = $Fa%9.5f Fb = $Fb%9.5f x = $x%9.5f Fx = $Fx%9.5f y = $y%9.5f Fy = ${phi(y)}%9.5f")
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
        Fy = phi(y)
      } else {
        a = x
        Fa = phi(a)
      }
      x = y - (-3*d-l+4*y)*pow((-l+y),2)/(6*(d+l-2*y)*(l-y))
      Fx = phi(x)
    }
    println(f"a = $a%9.5f b = $b%9.5f Fa = $Fa%9.5f Fb = $Fb%9.5f x = $x%9.5f Fx = $Fx%9.5f y = $y%9.5f Fy = ${phi(y)}%9.5f")
  }
}
