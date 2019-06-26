package fr.xebia.xke.fp3

sealed trait Retry[+T] {

  //TODO EXO7
  def flatMap[B](f: (T => Retry[B])): Retry[B] = Monade.retryMonade.flatMap(this)(f)

  //TODO EXO7
  def map[B](f: (T => B)): Retry[B] = Monade.retryMonade.map(this)(f)

  //TODO EXO7
  def filter(predicate: (T => Boolean)): Retry[T] = this match {
    case r@Success(t, tries) if predicate(t) => r
    case _=> Failure(new IllegalArgumentException)
  }

}

case class Failure(t: Throwable) extends Retry[Nothing]

case class Success[T](t: T, tries: Int = 0) extends Retry[T]

object Retry {

  //TODO EXO5
  def apply[T](retries: Int, tries: Int = 0)(t: () => T): Retry[T] =
    try {
      val r = t()
      if(retries > 1) {
        apply(retries - 1, tries)(t)
      }
      else{
        Success(r, tries)
      }
    }
  catch{
    case _ if retries > 1 => apply(retries-1, tries+1)(t)
    case e => Failure(e)
  }

}