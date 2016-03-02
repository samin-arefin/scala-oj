package string

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  * Created by nimas on 3/2/16.
  */
case class KMP(val pat: String) {
  val prefixTable = preProcess(0, 1, ArrayBuffer(0))

  def search(txt: String): Int = {
    @tailrec def loop(patIdx: Int, txtIdx: Int, matchLen: Int): Int = {
      if (matchLen == pat.length) txtIdx - matchLen
      else if (txtIdx >= txt.length) -1
      else if (pat(patIdx) == txt(txtIdx)) loop(patIdx + 1, txtIdx + 1, matchLen + 1)
      else if (matchLen == 0) loop(0, txtIdx + 1, 0)
      else loop(0, txtIdx - prefixTable(matchLen - 1), 0)
    }

    loop(0, 0, 0)
  }

  @tailrec private def preProcess(l: Int, r: Int, acc: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    if (r >= pat.length) acc
    else if (pat(l) == pat(r)) preProcess(l + 1, r + 1, acc += (l + 1))
    else preProcess(0, r + 1, acc += 0)
  }

}
