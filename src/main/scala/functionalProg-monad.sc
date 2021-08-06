/**
 * Functor: wrapping a value in a context, a kind of functional representation of different
 * Types which can be mapped over
 * */

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  /** implementing the unzip functor */
  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
}
implicit val listFunctor: Functor[List] = new Functor[List] {
  def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
}
def inc(list: List[Int])(implicit func: Functor[List]) = func.map(list)(_ + 1)
def dist(list: List[(Int,Int)])(implicit func: Functor[List]) = func.distribute(list)

inc(List(10,20,30))
dist(List((10,20),(15,30)))

/** Concatenating computations on the same monad(Option) */
val result: Option[Int] = for {
  first <- Option(1)
  second <- Option(2)
  third <- Option(3)
} yield first + second + third

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]
  def map[A,B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))
}

import java.util.concurrent._
/**
 * ref. executor service:
 * https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/concurrent/ExecutorService.html
 * provides a pool of threads and an API for assigning tasks to it.
 *
 * */

type Par[A] = ExecutorService => Future[A]

object Par {
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  /** running Par in a threat */
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def flatMap[A,B](p: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })
}


/***

val parMonad = new Monad[Par] {
  def unit[A](a: => A) = Par.unit(a)
  def flatMap[A,B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
}
def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
  def unit[A](a: => A) = p.succeed(a)
  def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
}
val optionMonad = new Monad[Option] {
  def unit[A](a: => A) = Some(a)
  def flatMap[A,B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
}
val streamMonad = new Monad[Stream] {
  def unit[A](a: => A) = Stream(a)
  def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
}
val listMonad = new Monad[List] {
  def unit[A](a: => A) = List(a)
  def flatMap[A,B](ma: List[A])(f: A => List[B]) = ma flatMap f
}

**/