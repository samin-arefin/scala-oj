package template

import java.io._
import java.util.StringTokenizer

import scala.annotation.implicitNotFound
import scala.language.postfixOps
import scala.reflect.ClassTag


/**
  * Created by nimas on 12/24/15.
  */
class IOTemplate(val delim: String = " \t",
                 private val input: InputStream = System.in,
                 private val output: OutputStream = System.out) extends Iterator[String] {

  val in  = new BufferedReader(new InputStreamReader(input))
  val out = new PrintWriter(new BufferedWriter(new OutputStreamWriter(output)))

  @implicitNotFound("No member of type class ParseAble in scope for ${T}")
  trait ParseAble[T] { val parse: StringTokenizer => T }

  object ParseAble {

    def apply[T](f: StringTokenizer => T) = new ParseAble[T] { override val parse: StringTokenizer => T = f(_) }

    object ParseAbleLine extends ParseAble[String] { override val parse: StringTokenizer => String = _ nextToken "\n" }

    implicit val ParseAbleString = new ParseAble[String] { override val parse: (StringTokenizer) => String = _ nextToken() }
    implicit val ParseAbleInt    = new ParseAble[Int]    { override val parse: (StringTokenizer) => Int = _ nextToken() toInt }
    implicit val ParseAbleLong   = new ParseAble[Long]   { override val parse: (StringTokenizer) => Long = _ nextToken() toLong }
    implicit val ParseAbleDouble = new ParseAble[Double] { override val parse: (StringTokenizer) => Double = _ nextToken() toDouble }
    implicit val ParseAbleBigInt = new ParseAble[BigInt] { override val parse = (strTok: StringTokenizer) => BigInt(strTok nextToken()) }

  }

  private val it = Iterator.iterate(Option(new StringTokenizer(""))) {
    case Some(strTok) =>
      if (strTok.hasMoreTokens) Some(strTok)
      else {
        val line = in.readLine()
        if (line == null) None
        else Some(new StringTokenizer(line, delim))
      }
    case _ => None
  }.buffered

  override def hasNext: Boolean = it.head match {
    case Some(tok) =>
      if (tok.hasMoreTokens) true
      else {
        it.next()
        hasNext
      }
    case None => false
  }

  override def next = read[String]

  def read[T](implicit parser: ParseAble[T]) = it
    .find {
      case Some(strTok) => strTok.hasMoreTokens
      case None => sys.error("nothing found of such type")
    }
    .collect { case Some(strTok) => parser parse strTok } get

  def readLine = read(ParseAble.ParseAbleLine)

  def readAll[T: ParseAble](n: Int)(implicit ev: ClassTag[T]) = (Array fill n) { read[T] }

  def close(): Unit = {
    in.close()
    out.close()
  }

}
