package string

import scala.annotation.tailrec

/**
  * Created by nimas on 3/3/16.
  */
case class BoyerMoore(pat: String, last: Char = 255, first: Char = 0) {

  val rightMost = Array.fill(last - first + 1) { -1 }
  pat.zipWithIndex.foreach { case (c, i) => rightMost(c) = i }

  def search(txt: String): Int = {

    @tailrec def loop(patIdx: Int, txtIdx: Int, matchLen: Int): Int = {
      if (matchLen == pat.length) txtIdx + 1
      else if (txtIdx >= txt.length) -1
      else if (pat(patIdx) == txt(txtIdx)) loop(patIdx - 1, txtIdx - 1, matchLen + 1)
      else if (rightMost(txt(txtIdx)) == -1) loop(pat.length - 1, txtIdx + pat.length, 0)
      else loop(pat.length - 1, txtIdx + pat.length - rightMost(txt(txtIdx)) - 1, 0)
    }

    loop(pat.length - 1, pat.length-1, 0)
  }

}
