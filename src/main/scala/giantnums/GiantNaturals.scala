/**
 * This package tests out ideas about a tree-based number
 * representation that can host gigantic numbers, for instance
 * "towers of exponents" and sparse combinations of them,
 * in a compressed form.
 *
 * It also provides compact representation for numbers in their close
 * neighborhoud like Mersenne and Fermat numbers as well as numbers
 * derived from them like the "perfect numbers", equal to the
 * sum of their proper divisors.
 *
 * For instance, the largest known prime number (at the end of year 2012),
 * the Mersenne prime 2^43112609 − 1 is represented as a small tree:
 *
 * V(W(T,[
 *   V(V(T,[]),[]),T,T,T,W(T,[]),V(T,[]),T,W(T,[]),W(T,[]),T,V(T,[]),T,T
 * ]),[])
 *
 * in contrast with the millions digits needed by its
 * conventional representation.
 *
 * Towards this end, we are using a "run-length" compressed
 * bijective base-2 representation for both 0 and 1 symbols.
 *
 * We see adding a 0 (represented as O) and 1 (represented as I)
 * as the application of a function.
 *
 * Numbers in bijective base 2 can be seen as finite strings of
 * made of the symbols O and I, elements of {O,I}*. O and I will
 * be seen here as generalized "constructors/extractors" using
 * Scala's apply/unapply methods.
 *
 * The key intuition is that each application of O(x) corresponds to
 * the function x->2x+1 and each application of I(x) to the function
 * x->2x+2. Our representation can be seen as describing natural
 * numbers in terms of itarated applications of these functions.
 *
 * In terms of the data type AlgT = T | V AlgT [AlgT] | W AlgT [AlgT]
 * - T represents the empty sequence
 * - V x xs represents an alternation of Os and Is
 *   where the first is an O, such x and each element of xs
 *   are counters starting with 0 (represented as T)
 * - W x xs represents an alternation of Is and Os
 *   where the first is an I, such x and each element of xs
 *   are counters starting with 0 (represented as T)
 *
 *   the "counters" are recursively represented the same way,
 *   such that the resulting trees/DAGs have internal nodes of type
 *   V or W and leves of type T
 *
 * We first implement various arithmetic operations on unbounded
 * natural numbers and then derive, using their Calkin-Wilf bijection
 * to rational numbers, and an extension to signed rationals
 * a possibly practical complete set of operations on them.
 * The bijection with naturals ensures that rational numbers are
 * stored at their information theoretical miniumum.
 *
 * The main open question is if, by using our representations,
 * there are significantly faster algorithms for some interesting
 * arithmetic functions and if interesting natural numbers or sequences
 * have significantly more compact forms.
 */
package giantnums

// primitive constructors/destructors/recognizers

final case object T extends AlgT
final case class V(x: AlgT, xs: List[AlgT]) extends AlgT {
  override def toString = {
    "V(" + x + "," + AlgT.showList(xs) + ")"
  }
}
final case class W(x: AlgT, xs: List[AlgT]) extends AlgT {
  override def toString = {
    "W(" + x + "," + AlgT.showList(xs) + ")"
  }
}

// derived "generalized" constructors/destructors/recognizers

/**
 * definition of successor/predecessor S
 * representing x<->x+1 on trees of type AlgT
 *
 * this is a O(1) operation - proof a bit
 * tedious, it follows from the fact that
 * the average numbers of 0's (or 1's) a
 * (bijective base-2) number ends with is ~ 1
 */
final object S extends AlgT {
  def apply(x: AlgT) = cS(x)

  def unapply(x: AlgT) = x match {
    case T => None
    case _ => Some(dS(x))
  }
}

/**
 * defintion of "generalized" constructor/destructor/recognizer O
 * representing x<->2*x+1 on trees of type AlgT
 *
 * a O(1) operation as cO,dO ~ cS, dS
 */
final object O extends AlgT {
  def apply(x: AlgT) = cO(x)

  def unapply(x: AlgT) = x match {
    case V(_, _) => Some(dO(x))
    case _       => None
  }
}

/**
 * defintion of "generalized" constructor/destructor/recognizer I
 * representing x<->2*x+2 on trees of type AlgT
 *
 * a O(1) operation as cI,dI ~ cS, dS
 *
 */
final object I extends AlgT {
  def apply(x: AlgT) = cI(x)

  def unapply(x: AlgT) = x match {
    case W(_, _) => Some(dI(x))
    case _       => None
  }
}

/**
 * this trait implements a number of
 * constant time (on the average) operations
 */
trait AlgT {

  //def isE(x: AlgT): Boolean = x == T
  //def isO(x: AlgT) = x.isInstanceOf[V]
  //def isI(x: AlgT) = x.isInstanceOf[W]

  /**
   * constructor seen as adding a 0 digit to the
   * least significant end of a bijective base-2
   * number
   */
  def cO(n: AlgT): AlgT = n match {
    case T        => V(T, Nil)
    case V(x, xs) => V(cS(x), xs)
    case W(x, xs) => V(T, x :: xs)
  }

  /**
   * constructor seen as adding a 1 digit to the
   * least significant end of a bijective base-2
   * number
   */
  def cI(n: AlgT): AlgT = n match {
    case T        => W(T, Nil)
    case V(x, xs) => W(T, x :: xs)
    case W(x, xs) => W(cS(x), xs)
  }

  /**
   * extractor, seen as removing a 0 digit from the
   * least significant end of a bijective base-2
   * number
   */
  def dO(n: AlgT): AlgT = n match {
    case V(T, Nil)     => T
    case V(T, x :: xs) => W(x, xs)
    case V(x, xs)      => V(dS(x), xs)
  }

  /**
   * extractor, seen as removing a 1 digit from the
   * least significant end of a bijective base-2
   * number
   */
  def dI(n: AlgT): AlgT = n match {
    case W(T, Nil)     => T
    case W(T, x :: xs) => V(x, xs)
    case W(x, xs)      => W(dS(x), xs)
  }

  /**
   * constructor seen as adding 1 to a
   * bijective base-2 number, avg.
   * effort O(1) ~ avg. number of 1s a
   * number ends with
   */
  def cS(n: AlgT): AlgT = n match {
    case T       => V(T, Nil)
    case V(_, _) => cI(dO(n))
    case W(_, _) => cO(cS(dI(n)))
  }

  /**
   * extractor, seen as subtracting 1 from a
   * bijective base-2 number, when possible,
   * i.e. when not the empty sequence
   *
   * effort O(1) ~ avg. number of 1s a
   * number ends with
   */
  def dS(n: AlgT): AlgT = n match {
    case V(T, Nil) => T
    case V(_, _)   => cI(dS(dO(n)))
    case W(_, _)   => cO(dI(n))
  }
}

/**
 * companion object for AlgT
 */
private final object AlgT {
  /**
   * improves a bit on the verbose
   * representation of lists in Scala
   */
  def showList(xs: List[AlgT]): String = {
    if (xs == Nil) "[]"
    else {
        def f(x: Char) = x != ' '

      "[" + xs.toString.drop(5).dropRight(1).filter(f) + "]"
    }
  }
}

/**
 * arithmetic computations with objects from AlgT
 */
trait GiantNaturals extends AlgT {

  /**
   * converts a BigInt representing a natural number
   * to its bijectively related tree representation
   */
  def n2t(n: BigInt): AlgT = n match {
    case x: BigInt if x == 0                  => T
    case x: BigInt if (x > 0 && (x & 1) == 1) => cO(n2t((x - 1) >> 1))
    case x: BigInt if (x > 0 && (x & 1) == 0) => cI(n2t((x - 2) >> 1))
  }

  /**
   * converts a tree representation of a natural number
   * to its bijectively related (non-negative) BigInt representation
   */
  def t2n(t: AlgT): BigInt = t match {
    case T       => 0
    case V(_, _) => (t2n(dO(t)) << 1) + 1
    case W(_, _) => (t2n(dI(t)) << 1) + 2
  }

  // comparison returns

  val LT = -1
  val EQ = 0
  val GT = 1

  /**
   * strengthens EQ into newrel
   */
  private def strengthen(rel: Int, newrel: Int) = rel match {
    case EQ => newrel
    case _  => rel
  }

  /**
   * comparaison function returning LT,EQ,GT
   */
  def cmp(u: AlgT, v: AlgT): Int = (u, v) match {
    case (T, T)       => EQ
    case (T, _)       => LT
    case (_, T)       => GT
    case (O(x), O(y)) => cmp(x, y)
    case (I(x), I(y)) => cmp(x, y)
    case (O(x), I(y)) => strengthen(cmp(x, y), LT)
    case (I(x), O(y)) => strengthen(cmp(x, y), GT)
  }

  /**
   * addition operation on binary trees, corresponding
   * to addition on N
   * - termination proof obvious: structural recursion
   */
  def add(u: AlgT, v: AlgT): AlgT = (u, v) match {
    case (T, y)       => y
    case (x, T)       => x
    case (O(x), O(y)) => I(add(x, y))
    case (O(x), I(y)) => O(S(add(x, y)))
    case (I(x), O(y)) => O(S(add(x, y)))
    case (I(x), I(y)) => I(S(add(x, y)))
  }

  /**
   * subtraction operation on binary trees, corresponding
   * to subtraction on N
   */
  def sub(u: AlgT, v: AlgT): AlgT = (u, v) match {
    case (x, T)       => x
    case (O(x), O(y)) => dS(O(sub(x, y)))
    case (O(x), I(y)) => dS(dS(O(sub(x, y))))
    case (I(x), O(y)) => O(sub(x, y))
    case (I(x), I(y)) => dS(O(sub(x, y)))
  }

  /*
   * double of t
   * 
   */
  def db(t: AlgT) = dS(cO(t))

  /**
   * exponent of 2 : a constant avg. time operation
   */
  def exp2(t: AlgT): AlgT = t match {
    case T    => V(T, Nil)
    case S(x) => S(V(x, Nil))
    //case x => cS(V(dS(x), Nil))
  }

  /**
   * multiplication operation on binary trees, corresponding
   * to multiplication on N
   */
  def mul(u: AlgT, v: AlgT): AlgT = (u, v) match {
    case (T, _) => T
    case (_, T) => T
    case (S(a), S(b)) => {
        def m(x: AlgT, y: AlgT): AlgT = x match {
          case T    => y
          case O(t) => O(m(t, y))
          case I(t) => S(add(y, O(m(t, y))))
        }
      S(m(a, b))
    }
  }

  /**
   * power operation on binary trees, corresponding
   * to u at exponent v on N
   */
  def pow(u: AlgT, v: AlgT): AlgT = (u, v) match {
    case (_, T)    => V(T, Nil)
    case (x, O(y)) => mul(x, pow(mul(x, x), y))
    case (x, I(y)) => {
      val xx = mul(x, x)
      mul(xx, pow(xx, y))
    }
  }

  /**
   * division with remainder operation on binary trees, corresponding
   * to the same on N
   */
  def div_and_rem(x: AlgT, y: AlgT): (AlgT, AlgT) =
    if (cmp(x, y) == LT) (T, x)
    else if (T == y) null // division by zero
    else {
        //println("div>>" + toN(x) + "/" + toN(y))
        def try_to_double(x: AlgT, y: AlgT, k: AlgT): AlgT =
          if (cmp(x, y) == LT) dS(k)
          else try_to_double(x, db(y), S(k))

        def divstep(n: AlgT, m: AlgT): (AlgT, AlgT) = {
          val q = try_to_double(n, m, T)
          val p = mul(exp2(q), m)
          (q, sub(n, p))
        }
      val (qt, rm) = divstep(x, y)
      val (z, r) = div_and_rem(rm, y)
      val dv = add(exp2(qt), z)
      (dv, r)
    }

  /**
   * division
   */
  def div(x: AlgT, y: AlgT) = div_and_rem(x, y)._1

  /**
   * reminder
   */
  def rem(x: AlgT, y: AlgT) = div_and_rem(x, y)._2

  /**
   * greatest common divisor
   */
  def gcd(x: AlgT, y: AlgT): AlgT = if (y == T) x else gcd(y, rem(x, y))

  /**
   * least common multiplier
   */
  def lcm(x: AlgT, y: AlgT): AlgT = mul(div(x, gcd(x, y)), y)

  /**
   * computes (b^exp) modulo m
   */
  def modPow(b: AlgT, exp: AlgT, m: AlgT) = {
    var result: AlgT = V(T, Nil)
    var exponent = exp
    var base = b
    while (exponent != T) {
      exponent match {
        case O(x) => {
          result = rem(mul(result, base), m)
          exponent = x
        }
        case I(x) => exponent = S(x)
      }
      base = rem(mul(base, base), m)
    }
    result
  }

  // a few small constants

  // val ZERO = T
  val ONE = V(T, List())
  val TWO = W(T, List())
  val THREE = V(V(T, List()), List())
  val FOUR = W(T, List(T))
  val FIVE = V(T, List(T))
  val SIX = W(V(T, List()), List())
  val SEVEN = V(W(T, List()), List())

  // our own random generator

  val ranA = n2t(16807)
  val ranC = T
  val ranM = dS(exp2(n2t(31)))

  var ranR = ranA

  /**
   * sets the seed of our random generator
   */
  def setSeed(seed: AlgT) {
    ranR = add(ranC, seed)
  }

  /**
   * returns a random number between
   * 0 and limit-1
   */
  def random(limit: AlgT): AlgT = {
    ranR = rem(add(mul(ranA, ranR), ranC), ranM)
    rem(ranR, limit)
  }

  /**
   * half of an even number
   */
  def hf(x: AlgT) = x match { case I(x) => S(x) }

  /**
   * smallest exponent of 2 that divides x
   */
  def diadicValuation(x: AlgT): AlgT = x match {
    case I(x) => S(diadicValuation(S(x)))
    case O(_) => T
  }

  /**
   * the dual represents a number in which Os and Is are flipped
   * it is constant time with our representation
   *
   * a constant time operation
   *
   */
  def dual(t: AlgT): AlgT = t match {
    case T        => T
    case V(x, xs) => W(x, xs)
    case W(x, xs) => V(x, xs)
  }

  /**
   * deterministic step in Miller-Rabin primality test
   */
  def millerRabinStep(a: AlgT, n: AlgT): Boolean = {

    var d: AlgT = dS(n)
    var e: AlgT = diadicValuation(d)
    var aPow: AlgT = T
    var i: AlgT = ONE

    aPow = modPow(a, d, n)
    if (aPow == ONE) true
    else {
      while (i != e) {
        if (aPow == dS(n)) return true
        aPow = rem(mul(aPow, aPow), n)
        i = S(i)
      }
      aPow == dS(n)
    }
  }

  /**
   * Miller-Rabin primality test for n
   * with (1/4)^k chances of being wrong
   */
  def millerRabin(k: AlgT, n: AlgT): Boolean = n match {
    case T                      => false //0
    case ONE                    => false //1
    case TWO                    => true // 2
    case W(_, _) if !(n == TWO) => false //even>2: 4,..
    case THREE                  => true // 3
    case x => if (rem(x, THREE) == T) false // div by 3, > 3
    else {
      var i: AlgT = T
      val m = dS(dS(dS(dS(n))))
      while (cmp(i, k) == LT) {
        val r = S(S(random(m)))
        if (millerRabinStep(r, n)) i = S(i)
        else return false
      }
      true
    }
  }

  /**
   *  Lucas-Lehmer fast primality test for Mersenne numbers
   */
  def lucasLehmer(p: AlgT): Boolean = {
    val p2 = dS(dS(p))
    val m = exp2(p)

      def f(k: AlgT, n: AlgT): AlgT = {
        if (T == k) n
        else {
          val x = f(dS(k), n)
          val y = dS(dS(mul(x, x)))
          fastmod(y, m)
        }
      }
      def fastmod(k: AlgT, m: AlgT): AlgT = {
        if (k == dS(m)) T
        else if (LT == cmp(k, m)) k
        else {
          val (q, r) = div_and_rem(k, m)
          fastmod(add(q, r), m)
        }
      }

    T == f(p2, FOUR)
  }

  /**
   * list of probable primes in the interval  [from,from+k]
   */
  def probablePrimes(from: AlgT, k: AlgT): List[AlgT] = {
    val kN = t2n(k)
    val fromN = t2n(from)
    val toN = fromN + kN
    val mrSteps = n2t(20)
    var ps: List[AlgT] = Nil
    for (i <- fromN to toN) {
      if (i < 6 || (!((i & 1) == 0 || i % 3 == 0 || i % 5 == 0))) {
        val p = n2t(i)
        if (millerRabin(mrSteps, p))
          ps = p :: ps
      }
    }
    ps.reverse
  }

  // Fermat, Mersenne and Perfect numbers have all simple expressions!!!

  /**
   * n-the Fermat number
   */
  def fermat(n: AlgT) = cS(exp2(exp2(n)))

  /**
   * a Mersenne number associated to prime p
   */
  def mersenne(p: AlgT) = dS(exp2(p))

  /**
   * an (even) perfect number associated to prime p,
   * i.e. number such that the sum of its proper
   * divisors equals it
   *
   * surprisingly, computing this is constant time
   * operation
   */
  def perfect(p: AlgT) = {
    val q = dS(dS(p))
    cS(V(q, List(q)))
  }

  /**
   * generates the exponents of Mersenne primes up to limit
   */
  def mersennePrimeExps(limit: AlgT) = {
    val ps = probablePrimes(ONE, limit)
    ps.filter(lucasLehmer)
  }

  /**
   * generates the actual Mersenne primes for exponents
   * up to limit
   */
  def mersennePrimes(expLimit: AlgT) = {
    mersennePrimeExps(expLimit).map(mersenne)
  }

  // largest known prime (a Mersenne number):

  /**
   * the prime used as exponent for the largest
   * known prime number up to december 2012
   */
  val prime45 = 43112609

  /**
   * the largest known prime number up to december 2012,
   * a Mersenne numbe
   */

  val mersenne45 = dS(exp2(n2t(prime45)))

  /**
   * the largest known perfect number up to december 2012,
   * derived from the largest known Mersenne prime
   */

  val perfect45 = perfect(n2t(prime45))

}

