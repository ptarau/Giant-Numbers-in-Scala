package giantnums

object Main extends App with GiantNumTester {
  // comment/uncomment as needed
  val r: AlgT = add(n2t(3), n2t(10))
  println("3+10=>" + r + "=>" + t2n(r))

  //t1
  //t2

  //qtest

  //millerTest(BigInt(1) << 32) // 23 secs
  //millerTest(prime45)
  for (t <- probablePrimes(n2t(1), n2t(16)))
    println("prime=" + (t, t2n(t)))

  for (t <- mersennePrimes(n2t(32)))
    println("mersennePrime=" + (t, t2n(t)))

  println("largest known Mersenne prime=2^" + prime45 + "-1:\n" + mersenne45)
  println("largest known even perfect number=\n" + perfect45)
  println("largest known Fermat prime =\n" + fermat(n2t(4)))
}
