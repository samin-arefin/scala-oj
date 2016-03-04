package string

/**
  * Created by nimas on 3/4/16.
  */
object Util {
  def hashString(key: String)(len: Int)(alphabetSize: Int)(prime: Long): Long = {
    key
      .take(len)
      .foldLeft(0l) { case (h, c) =>
        (alphabetSize * h + c) % prime
      }
  }
}
