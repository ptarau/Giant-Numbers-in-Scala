package giantnums
import scala.util.Random

trait GiantNumTester extends GiantRationals {
  var lastTime: Long = System.currentTimeMillis

  def time = {
    val t = System.currentTimeMillis
    val d = t - lastTime
    lastTime = t
    d
  }

  def time(f: => Any) = {
    val s = System.currentTimeMillis
    f
    val t = System.currentTimeMillis - s
    t
  }

  def op1(f: AlgT => AlgT, x: BigInt) = {
    t2n(f(n2t(x)))
  }

  def optest1(opname: String, f: AlgT => AlgT, x: BigInt) {
    println(opname + "(" + x + ")" + " = " + op1(f, x))
  }

  def op2(f: (AlgT, AlgT) => AlgT, x: BigInt, y: BigInt) = {
    t2n(f(n2t(x), n2t(y)))
  }

  def optest2(opname: String, f: (AlgT, AlgT) => AlgT, x: BigInt, y: BigInt) {
    println(x + " " + opname + " " + y + " = " + op2(f, x, y))
  }

  def t2 {
    val x = 29
    val y = 10
    val z = 6
    optest2("+", add, x, y)
    optest2("+", add, x, z)
    optest2("+", add, y, z)
    optest2("-", sub, x, y)
    optest2("-", sub, x, z)
    optest2("-", sub, y, z)
    optest2("*", mul, x, y)
    optest2("*", mul, x, z)
    optest2("*", mul, y, z)
    optest1("exp2", exp2, y)
    optest1("exp2", exp2, y)
    optest2("^", pow, x, y)
    optest2("^", pow, x, z)
    optest2("^", pow, y, z)
    optest2("div", div, x, y)
    optest2("div", div, x, z)
    optest2("div", div, y, z)
    optest2("%", rem, x, y)
    optest2("%", rem, x, z)
    optest2("%", rem, y, z)

  }

  def t1 {
    var x: AlgT = T
    for (i <- 0 to 3) {
      println(i + ":" + x)
      x = S(x)
    }
    println("-----")
    for (i <- 0 to 10) {
      val t = n2t(i)
      val j = t2n(t)
      println(i + "=" + j + ":" + t)
    }
    println("-----")
    for (i <- 0 to 10) {
      val t = n2t(i)
      val x = exp2(t)
      val j = t2n(x)
      println("exp2:" + i + "=>" + j + ":" + x)
      if (i > 0) {
        val v = diadicValuation(t)
        val k = t2n(v)
        println("dval:" + i + "=>" + k + ":" + v)
      }
    }
  }

  def millerTest(n: BigInt) {
    val lim = exp2(n2t(31))
    val k = 24

    for (i <- 0 to 3) {
      println(t2n(random(lim)))
    }
    var t1: Long = 0L
    var t2: Long = 0L
    for (i <- (n - 100).abs to n + 200) {
      time
      val ref = i.isProbablePrime(k)
      t1 += time
      if (ref) println("BigInt prob. prime=" + i)
      time
      val ok = millerRabin(n2t(k), n2t(i))
      t2 += time
      if (ok) println("AlgT prob. prime=" + i + "\n")
    }
    println("time BigInt=" + t1)
    println("time AlgT=" + t2)
  }

  val qrand = new Random(2012)
  val bits = 8
  def randomN(): BigInt = BigInt(bits, qrand)
  def randomT(): AlgT = n2t(randomN())
  def randomQ(): Q = fromT(randomT())

  def qtest {
    var i = 0
    while (i < 10) {
      val x = randomQ()
      val y = randomQ()

      println("x=" + x.toFraq() + ", y=" + y.toFraq)
      val s = radd(x, y)
      val y1 = rsub(s, x)

      println("+,- " + y.toFraq() + "=" + y1.toFraq())

      val p = rmultiply(x, y)
      val y2 = rdivide(p, x)

      println("*,/ " + y.toFraq() + "=" + y2.toFraq())

      i += 1
    }
  }

}
