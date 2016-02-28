package template

import java.io._
import java.util.StringTokenizer

import scala.annotation.implicitNotFound
import scala.language.postfixOps
import scala.reflect.ClassTag


/**
  * Created by nimas on 12/24/15.
  */
class IOTemplate(val delim: String = " ",
                 val input: InputStream = System.in,
                 val output: OutputStream = System.out) extends Iterator[String] {

  val in  = new BufferedReader(new InputStreamReader(input))
  val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(output)))

  @implicitNotFound("No member of type class ParseAble in scope for ${T}")
  trait ParseAble[T] {
    def parse(strTok: StringTokenizer): T
  }

  object ParseAble {

    def apply[T](f: StringTokenizer => T) = new ParseAble[T] {
      override def parse(strTok: StringTokenizer): T = f(strTok)
    }

    object ParseAbleLine extends ParseAble[String] {
      override def parse(strTok: StringTokenizer): String = strTok nextToken "\n"
    }

    implicit object ParseAbleString extends ParseAble[String] {
      override def parse(strTok: StringTokenizer): String = strTok nextToken()
    }

    implicit object ParseAbleInt extends ParseAble[Int] {
      override def parse(strTok: StringTokenizer): Int = strTok nextToken() toInt
    }

    implicit object ParseAbleLong extends ParseAble[Long] {
      override def parse(strTok: StringTokenizer): Long = strTok nextToken() toLong
    }

    implicit object ParseAbleDouble extends ParseAble[Double] {
      override def parse(strTok: StringTokenizer): Double = strTok nextToken() toDouble
    }

    implicit object ParseAbleBigInt extends ParseAble[BigInt] {
      override def parse(strTok: StringTokenizer): BigInt = BigInt(strTok nextToken())
    }

  }

  val it = Iterator.iterate(Option(new StringTokenizer(""))) {
    case Some(strTok) =>
      if (strTok.hasMoreTokens) Some(strTok)
      else {
        val line = in.readLine()
        if (line == null) None
        else Some(new StringTokenizer(line, delim))
      }
    case _ => None
  }.buffered

  def hasNext: Boolean = it.head match {
    case Some(tok) =>
      if (tok.hasMoreTokens) true
      else {
        it.next()
        hasNext
      }
    case None => false
  }

  def next = read[String]

  def readLine = read(ParseAble.ParseAbleLine)

  def readAll[T: ParseAble](n: Int)(implicit ev: ClassTag[T]) = (Array fill n) {
    read[T]
  }

  def read[T](implicit parser: ParseAble[T]) = it
    .find {
      case Some(strTok) => strTok.hasMoreTokens
      case None => throw new Exception("nothing found of such type")
    }
    .collect { case Some(strTok) => parser parse strTok } get

  def close(): Unit = {
    in.close()
    out.close()
  }

}
