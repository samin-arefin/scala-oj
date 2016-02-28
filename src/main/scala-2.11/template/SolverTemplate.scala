package template

import scala.language.reflectiveCalls

/**
  * Created by nimas on 2/21/16.
  */
object SolverTemplate extends App {


  class WithTimer(val name: String = "", procedure: => Unit) {
    private val start = System.currentTimeMillis()
    procedure
    private val end = System.currentTimeMillis()
    val time = end - start
    def foreach(f: (String, Long) => Unit) = f(name, time)
  }

  object WithTimer {
    def apply(name: String = "")(procedure: => Unit) = new WithTimer(name, procedure)
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

    WithTimer("some process") {


      // do your stuff

    }
  }

}
