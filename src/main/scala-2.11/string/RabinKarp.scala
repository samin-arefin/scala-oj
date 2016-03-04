package string

import essential.Mathematics._

import scala.annotation.tailrec

/**
  * Created by nimas on 3/4/16.
  */
case class RabinKarp(pat: String, last: Char = 255, first: Char = 0) {

  private val prime       : Long = randomPrime(55)
  private val alphabetSize: Int  = last - first + 1
  private val patHash     : Long = Util.hashString(pat)(pat.length)(alphabetSize)(prime)
  private val multiplier  : Long = Iterator.iterate(1l) { i =>
    (alphabetSize * i) % prime
  }.drop(pat.length - 1).next


  def search(txt: String): Int = {

    @tailrec def loop(lastHash: Long, idx: Int): Int =
      if (lastHash == patHash) idx - 1
      else if (idx + pat.length > txt.length) -1
      else {
        val powToLastIdx = (txt(idx - 1) * multiplier) % prime
        val truncated = (lastHash + prime - powToLastIdx) % prime
        val newHash = (truncated * alphabetSize + txt(idx + pat.length - 1)) % prime
        loop(newHash, idx + 1)
      }

    loop(Util.hashString(txt)(pat.length)(alphabetSize)(prime), 1)
  }

}
