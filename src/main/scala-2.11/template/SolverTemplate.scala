package template

import scala.language.reflectiveCalls

/**
  * Created by nimas on 2/21/16.
  */
object SolverTemplate extends App {


  class WithTimer(procedure: => Unit) {
    private val start = System.currentTimeMillis()
    procedure
    private val end = System.currentTimeMillis()
    val time = end - start
    def foreach(f: Long => Unit) = f(time)
  }

  object WithTimer {
    def apply(procedure: => Unit) = new WithTimer(procedure)
  }


  // taken from http://www.slideshare.net/Odersky/fosdem-2009-1013261 slide 21
  def using[T <: {def close()}]
  (resource: T)
  (block: T => Unit) {
    try {
      block(resource)
    } finally {
      if (resource != null) resource.close()
    }
  }


  using(new IOTemplate()) { io =>
    import io._

    WithTimer {


      // do your stuff

    }
  }

}
