package xyz.hyperreal.lia

import math.{sqrt => sqr, _}

object Util {
  val ZERObi = BigInt(0)
  val ONEbi = BigInt(1)

  def asinh(x: Double) = log(x + sqr(x * x + 1))

  def acosh(x: Double) = log(x + sqr(x * x - 1))

  def atanh(x: Double) = (log(1 + x) - log(1 - x)) / 2

  private lazy val lg2 = log10(2)

  def digits(n: BigInt) = (n.bitLength * lg2).toInt + 1

  def gcd(a: Int, b: Int) = {
    def _gcd(_a: Int, _b: Int): Int =
      if (_b == 0)
        _a
      else
        _gcd(_b, _a % _b)

    _gcd(abs(a), abs(b))
  }

  def egcd(a: Int, b: Int) = {
    def _egcd(a: Int,
              b: Int,
              s1: Int,
              s2: Int,
              t1: Int,
              t2: Int): (Int, Int, Int) =
      if (b == 0)
        (a, s2, t2)
      else {
        val q = a / b

        _egcd(b, a - q * b, s2 - q * s1, s1, t2 - q * t1, t1)
      }

    val res = _egcd(a, b, 0, 1, 1, 0)

    if (res._1 < 0)
      (-res._1, -res._2, -res._3)
    else
      res
  }

  def divides(a: Int, b: Int) = b % a == 0

  def even(a: Int) = divides(2, a)

  def odd(a: Int) = !even(a)

  def coprime(a: Int, b: Int) = gcd(a, b) == 1

  def coprime(a: Int, bs: List[Int]): Boolean = bs forall (coprime(a, _))

  def coprime(as: List[Int]): Boolean =
    coprime(as.head, as.tail) && (as.tail == Nil || coprime(as.tail))

  def mod(a: Int, m: Int) =
    if (a >= m)
      a % m
    else if (a < 0)
      m - (-a % m)
    else
      a

  def modinv(a: Int, m: Int) = {
    val (g, s, _) = egcd(a, m)

    if (g != 1) sys.error("modinv: no inverse")

    mod(s, m)
  }

  def crt(eqs: (Int, Int)*) = {
    if (eqs.length == 0) sys.error("crt: system is empty")

    val mods = eqs map (_._2) toList

    if (!coprime(mods))
      sys.error("crt: the moduli must all be pairwise coprime")

    val M = mods reduceLeft (_ * _)
    var s = 0

    for ((ai, mi) <- eqs) {
      val Mi = M / mi

      s += mod(ai * Mi * modinv(Mi, mi), M)
    }

    mod(s, M)
  }

  def nearly(x: Double, y: Double) = (x - y).abs < 1e-15

  def roughly(x: Double, y: Double) = (x - y).abs < 1e-13

  def pow(b: BigInt, e: BigInt) = {
    require(e >= ZERObi, "Util.pow: exponent must be non-negative")

    def _pow(b: BigInt, e: BigInt): BigInt =
      if (e == ONEbi)
        b
      else if (e testBit 0)
        b * _pow(b * b, (e - 1) / 2)
      else
        _pow(b * b, e / 2)

    if (b == ZERObi)
      ZERObi
    else if (e == ZERObi)
      ONEbi
    else
      _pow(b, e)
  }
}
