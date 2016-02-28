package essential


import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by nimas on 2/21/16.
  */
object Mathematics {


  @tailrec def gcd(i: Int, j: Int): Int =
    if (j == 0) i
    else gcd(j, i % j)


  def lcm(i: Int, j: Int): Int = (i / gcd(i, j)) * j


  def nCk(n: Int, k: Int): Long = {
    val newK = Math.min(k, n - k) // as nCk == nC(n-k); so take the minimum
    @tailrec def loop(i: Int, acc: Long): Long = {
      if (i > newK) acc
      else loop(i + 1, acc * (n - newK + i) / i)
    }
    loop(1, 1)
  }


  def nCkDP(size: Int): (Int, Int) => Long = {
    val xs = Array.fill[Long](size + 1, size + 1) {
      -1
    }
    def f(n: Int, k: Int): Long = {
      if (k == 1) n
      else if (n == k) 1
      else if (xs(n)(k) != -1) xs(n)(k)
      else {
        val x = f(n - 1, k - 1)
        val y = f(n - 1, k)
        xs(n)(k) = x + y
        xs(n)(k)
      }
    }
    f
  }

  def nPk(n: Int, k: Int): Long = {
    @tailrec def loop(i: Int, acc: Long): Long = {
      if (i == k) acc
      else loop(i + 1, acc * (n - i))
    }
    loop(0, 1)
  }


  def randomPrime(bitLength: Int): Long =
    if (bitLength < 2 || bitLength > 63) BigInt.probablePrime(31, new Random()).longValue()
    else BigInt.probablePrime(bitLength, new Random()).longValue()


  def bigSqrt(n: BigInt): BigInt = {
    var a: BigInt = 1
    var b: BigInt = (n >> 5) + 8
    while (b >= a) {
      val mid: BigInt = (a + b) >> 1
      if (mid * mid > n) b = mid - 1
      else a = mid + 1
    }
    a - 1
  }


  /**
    * @return (gcd, x, y); such x, y that ax + by = gcd
    */
  def extendedEuclid(a: Int, b: Int): (Int, Int, Int) = {
    if (b == 0) (a, 1, 0)
    else extendedEuclid(b, a % b) match {
      case (d, x, y) => (d, y, x - (a / b) * y)
    }
  }


  /**
    * @return -1 if a, b not coprime, otherwise the result
    */
  def inverseMod(a: Int, b: Int): Int = {
    extendedEuclid(a, b) match {
      case (d, _, _) if d != 1 => -1
      case (_, x, _) if x < 0 => x + b
      case (_, x, _) => x
    }
  }


  /**
    * @return a sieving function to determine primes in this size range
    */
  def sieve(size: Int): (Int) => Boolean = {
    val status = Array.ofDim[Int]((size >> 1) + 1)

    @tailrec def loop(x: Int): Unit = {
      if (x * x <= size) {
        if (status(x >> 1) == 0) {
          (x * x to size by 2 * x)
            .foreach {j => status(j >> 1) = 1}
        }
        loop(x + 2)
      }
    }

    loop(3)

    def isPrime(i: Int) = {
      if (i < 2) false
      else if (i == 2) true
      else if ((i & 1) == 0) false
      else if (status(i >> 1) == 1) false
      else true
    }

    isPrime
  }


  def eulerPhi(n: Int): Long = {
    getPrimeFactors(n)
      .distinct
      .foldLeft(n.toDouble) {(a, b) => {
        a * (1 - (1.0/b))
      }}
      .round
  }



  def getDivisors(n: Int): ArrayBuffer[Int] = {
    @tailrec def loop(d: Int, x: Int, ds: ArrayBuffer[Int]): ArrayBuffer[Int] =
      if (d * d >= x) ds
      else if (x % d == 0) loop(d + 1, x, ds += d += x / d)
      else loop(d + 1, x, ds)

    loop(1, n, if (Math.sqrt(n).isValidInt) ArrayBuffer(Math.sqrt(n).toInt) else ArrayBuffer.empty)
  }


  def getPrimeFactors(n: Int): ArrayBuffer[Int] = {
    @tailrec def loop(x: Int, d: Int, ps: ArrayBuffer[Int]): ArrayBuffer[Int] =
      if (d * d > x) ps += x
      else if (x % d == 0) loop(x / d, d, ps += d)
      else loop(x, if (d == 2) 3 else d + 2, ps)

    if (n > 0) loop(n, 2, ArrayBuffer.empty) else ArrayBuffer.empty
  }


  def countDivisors(n: Int): Int = {
    getPrimeFactors(n)
      .groupBy(identity)
      .foldLeft(1) {(a, b) => a * (b._2.length + 1)}
  }


  def isPerfectN(n: Int)(i: Int): Boolean = {
    getPrimeFactors(i)
      .groupBy(identity)
      .forall(t => t._2.length % n == 0)
  }


  def sumOfDivisors(n: Int): Long = {
    val ret = getPrimeFactors(n)
      .groupBy(identity)
      .foldLeft(1.0) { (x, y) => {
        val pow = Math.pow(y._1, y._2.length + 1) - 1
        x / (y._1 - 1) * pow
        }
      }
    if (n == 0) 0l else ret.round
  }


}
