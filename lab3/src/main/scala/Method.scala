import scala.collection.mutable.ArrayBuffer

/**
  * Created by NoName on 21.02.2016.
  */
object Method {

  var debug = false

  var da: ArrayBuffer[Array[Double]] = new ArrayBuffer[Array[Double]]()

  def printDebug = {
    val header = Array("k", "delta", "x", "fx", "y", "fy")
    for (col <- header) print(f"$col%9s")
    println
    var k = 1
    for (row <- da) {
      print(f"$k%9d")
      for (col <- row) print(f"$col%9.5f")
      println
      k += 1
    }
  }

  def apply(phi: (Double) => Double, a0: Double, b0: Double, lambda: Double, eps:Double): (Double, Double) = {
    var (a,b) = (a0,b0)

    var d = lambda * (b - a)
    var x = b - d
    d = lambda * d
    var y = x + d
    var (fx, fy) = (phi(x), phi(y))

    var k = 1
    while ((b - a) > eps) {
      da += Array(d, x, fx, y, fy)
      k += 1
      d = lambda * d
      if (fx <= fy) {
        b = y
        y = x
        fy = fx
        x = y - d
        fx = phi(x)
      }
      else {
        a = x
        x = y
        fx = fy
        y = x + d
        fy = phi(y)
      }
    }
    da += Array(d, x, fx, y, fy)
    if (debug) printDebug
    return if (fx <= fy) (x, fx) else (y, fy)
  }

}
