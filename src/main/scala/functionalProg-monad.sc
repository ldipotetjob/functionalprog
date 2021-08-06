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

  case class UnitFuture[A](get: A) extends Future[A] {
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
    es => es.submit(
      new Callable[A] {
        def call = a(es).get
      }
    )
}

/** Exc 11.1 */

val parMonad = new Monad[Par] {
  def unit[A](a: => A) = Par.unit(a)
  def flatMap[A,B](ma: Par[A])(f: A => Par[B]) = Par.flatMap(ma)(f)
}
/*** => TODO: Pending Parser Implementation
def parserMonad[P[+_]](p: Parsers[P]) = new Monad[P] {
  def unit[A](a: => A) = p.succeed(a)
  def flatMap[A,B](ma: P[A])(f: A => P[B]) = p.flatMap(ma)(f)
}
****/

/** TODO: Change Stream -> LazyList*/

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

def testMonadListMap (list: List[Int])(implicit mon: Monad[List]) = mon.map(list)(_+1)

/** testing monad unit*/
assert(listMonad.unit(1) == List(1))

listMonad.flatMap(List(12,34,55))(x=>List(Option(x)))

import State.unit
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1) })
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  // The idiomatic solution is expressed via foldRight
  def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  // This implementation uses a loop internally and is the same recursion
  // pattern as a left fold. It is quite common with left folds to build
  // up a list in reverse order, then reverse it at the end.
  // (We could also use a collection.mutable.ListBuffer internally.)
  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) = actions match {
      case Nil => (acc.reverse,s) case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
    }
    State((s: S) => go(s,sas,List()))
  }

  // We can also write the loop using
  // the previous solution, but it reverses the list _before_ folding it instead
  // of after. You might think that this is slower than the `foldRight` solution
  // since it walks over the list twice, but it's actually faster! The
  // `foldRight` solution technically has to also walk the list twice, since it
  // has to unravel the call stack, not being tail recursive. And the call stack // will be as tall as the list is long.
  def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
    l.reverse.foldLeft(unit[S, List[A]](List())) { (acc, f) => f.map2(acc)( _ :: _ )
    }
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}

/** Exc 11.2 */
/**  ref: State object come from Functional State */

//TODO MAKE A DEEPER ANALYSIS !!!!

class StateMonads[S] {
  type StateS[A] = State[S, A]
  // We can then declare the monad for the `StateS` type constructor:
  val monad = new Monad[StateS] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  } }

def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
  def unit[A](a: => A): State[S, A] = State(s => (a, s))
  override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
    st flatMap f
}

/** Exc 11.3 */
// TODO Explanation and test

  def sequence[A](ma: List[Functor[A]])(implicit monad: Monad[Functor]): Functor[List[A]] =
    ma.foldRight(monad.unit(List.empty[A])) { (ma, acc) =>
      monad.flatMap(ma) { a =>
        monad.map(acc) { as =>
          a :: as
        }
      }
    }

  def traverse[A, B](as: List[A])(f: A => Functor[B])(implicit monad: Monad[Functor]): Functor[List[B]] =
    as.foldRight(monad.unit(List.empty[B])) { (a, acc) =>
      monad.flatMap(f(a)) { b =>
        monad.map(acc) { bs =>
          b :: bs
        }
      }
    }

/** Exc 11.6 */

// ref matrix and cofactor: https://www.geeksforgeeks.org/minors-and-cofactors-of-determinants/
