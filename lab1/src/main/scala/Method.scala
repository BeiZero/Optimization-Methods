/**
  * Created by NoName on 18.02.2016.
  */
object Method {

  def curry(f: (Double, Double, Double) => Double, d: Double, l: Double): (Double) => Double = {
    f(_, d,l)
  }

  def run(f: (Double, Double, Double) => Double, d:Double, l:Double): (Double, Double, Double, Double, Double, Double) = {
    var x = 0
    var dx = 1
    val m = 2

    val phi_c = curry(f, d, l)

    var y = x + dx
    if(phi_c(y)>phi_c(x)){
      val z = x
      x = y
      y = z
      dx = -dx
    }
    while(phi_c(y)<=phi_c(x)){
      x = y
      dx = m*dx
      y = x + dx
    }
    val(a,b) = if(dx>0) (x-dx/m,y) else (y,x-dx/m)
    val result: (Double, Double, Double, Double, Double, Double) = (a, b, phi_c(x), phi_c(y), x, y)
    return result
  }
}
