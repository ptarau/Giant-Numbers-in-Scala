package giantnums

trait Q extends GiantRationals

case object Z extends Q
case class P(x: (AlgT, AlgT)) extends Q
case class M(x: (AlgT, AlgT)) extends Q

trait GiantRationals extends GiantNaturals {
  type PQ = (AlgT, AlgT)

  def toFraq(): (BigInt, BigInt) = this match {
    case Z         => (0, 1)
    case M((a, b)) => (-(t2n(a)), t2n(b))
    case P((a, b)) => (t2n(a), t2n(b))
  }

  /**
   * Rationals - first step:
   *
   * Variant of Dijkstra's fusc function from the Calkin-Wilf paper
   * providing a constructive bijection between Naturals and positive
   * rationals
   */
  /*
  def cw(u: AlgT): AlgT = u match {
    case T    => S(T)
    case O(x) => cw(x)
    case I(x) => add(cw(x), cw(S(x)))
  }
  */

  /**
   * splits u seen as a natural number into its
   * corresponding Calkin-Wilf rational, represented
   * as a pair
   */
  def t2pq(u: AlgT): PQ = u match {
    case T => (S(T), S(T))
    case O(n) => {
      val (x, y) = t2pq(n)
      (x, add(x, y))
    }
    case I(n) => {
      val (x, y) = t2pq(n)
      (add(x, y), y)
    }
  }

  /**
   * fuses back into a "natural number",
   * representing the path in the Calkin-Wilf tree,
   * a pair of co-prime natural numbers representing
   * the (numerator,denominator) of a positive rational number
   */
  def pq2t(uv: PQ): AlgT = uv match {
    case (O(T), O(T)) => T
    case (a, b) =>
      cmp(a, b) match {
        case GT => I(pq2t(sub(a, b), b))
        case LT => O(pq2t(a, sub(b, a)))
      }
  }

  /**
   * Signed Rationals from trees seen as natural numbers
   */
  def fromT(t: AlgT): Q = t match {
    case T    => Z // zero -> zero
    case O(x) => M(t2pq(x)) // odd -> negative
    case I(x) => P(t2pq(x)) // even -> positive
  }

  /**
   * From Signed Rationals to trees seen as natural numbers
   */
  def toT(q: Q): AlgT = q match {
    case Z    => T // zero -> zero
    case M(x) => O(pq2t(x)) // negative -> odd
    case P(x) => I(pq2t(x)) // positive -> even
  }

  /**
   * bijection from BigInt natural numbers to
   * tree-represented Signed Rationals
   */
  def nat2rat(n: BigInt): Q = fromT(n2t(n))

  /**
   * bijection from tree-represented Signed Rationals to
   * BigInt natural numbers
   */
  def rat2nat(q: Q): BigInt = t2n(toT(q))

  def fraq2pq(nd: (BigInt, BigInt)): PQ =
    pqsimpl((n2t(nd._1), n2t(nd._2)))

  def pq2fraq(nd: PQ): (BigInt, BigInt) =
    (t2n(nd._1), t2n(nd._2))

  /**
   * simplifies a positive fraction represented as a pair
   */
  def pqsimpl(xy: PQ) = {
    val x = xy._1
    val y = xy._2
    val z = gcd(x, y)
    (div(x, z), div(y, z))
  }

  /**
   * addition/subtraction template
   */
  def pqop(f: (AlgT, AlgT) => AlgT, xy: PQ, uv: PQ): PQ = {
    val (x, y) = xy
    val (u, v) = uv
    val z = gcd(y, v)
    val y1 = div(y, z)
    val v1 = div(v, z)
    val num = f(mul(x, v1), mul(u, y1))
    val den = mul(z, mul(y1, v1))
    pqsimpl((num, den))
  }

  def pqadd(a: PQ, b: PQ) = pqop(add, a, b)
  def pqsub(a: PQ, b: PQ) = pqop(sub, a, b)

  def pqcmp(xy: PQ, uv: PQ) = {
    val (x, y) = xy
    val (u, v) = uv
    cmp(mul(x, v), mul(y, u))
  }
  def pqmultiply(a: PQ, b: PQ) =
    pqsimpl(mul(a._1, b._1), mul(a._2, b._2))

  def pqinverse(a: PQ) = (a._2, a._1)

  def pqdivide(a: PQ, b: PQ) = pqmultiply(a, pqinverse(b))

  // operations on signed rationals

  def ropposite(x: Q) = x match {
    case Z    => Z
    case M(a) => P(a)
    case P(a) => M(a)

  }

  def radd(a: Q, b: Q): Q = (a, b) match {
    case (Z, y)       => y
    case (M(x), M(y)) => M(pqadd(x, y))
    case (P(x), P(y)) => P(pqadd(x, y))
    case (P(x), M(y)) => pqcmp(x, y) match {
      case LT => M(pqsub(y, x))
      case EQ => Z
      case GT => P(pqsub(x, y))
    }
    case (M(x), P(y)) => ropposite(radd(P(x), M(y)))
  }

  def rsub(a: Q, b: Q) = radd(a, ropposite(b))

  def rmultiply(a: Q, b: Q): Q = (a, b) match {
    case (Z, _)       => Z
    case (_, Z)       => Z
    case (M(x), M(y)) => P(pqmultiply(x, y))
    case (M(x), P(y)) => M(pqmultiply(x, y))
    case (P(x), M(y)) => M(pqmultiply(x, y))
    case (P(x), P(y)) => P(pqmultiply(x, y))
  }

  def rinverse(a: Q) = a match {
    case M(x) => M(pqinverse(x))
    case P(x) => P(pqinverse(x))
  }

  def rdivide(a: Q, b: Q) =
    rmultiply(a, rinverse(b))

}

