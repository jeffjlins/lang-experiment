package fpis.ch7

import java.util.concurrent._

import scala.language.implicitConversions

//class Par[A](f: => A) { }

object Par {

  //7.1 @Before
  //def unit[A](f: => A): Par[A] = { new Par(f) }

  //7.1
  //def map2[A, B, C](f1: => A, f2: => B)(f3: (A, B) => C): Par[C] = { ??? }

  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  // 7.3
  def map2Timeout[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      new Future[C] {
        def isDone = af.isDone && bf.isDone
        def get = f(af.get, bf.get)
        def get(timeout: Long, units: TimeUnit) = {
          val timeoutMillis = units.toMillis(timeout)
          val start = System.currentTimeMillis()
          val afg = af.get(timeout, units)
          val bfg = bf.get(timeoutMillis - (System.currentTimeMillis() - start), TimeUnit.MILLISECONDS)
          f(afg, bfg)
        }
        def isCancelled = af.isCancelled && bf.isCancelled
        def cancel(evenIfRunning: Boolean): Boolean = af.cancel(evenIfRunning) || bf.cancel(evenIfRunning)
      }
    }
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

//  def asyncF[A, B](f: A => B): A => Par[B] = {
//    (a: A) => (e: ExecutorService) => UnitFuture(f(a))
//  }

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

  def sortPar2(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List()))((pa, pb) => map2(pa, pb)(_ :: _))
  }

  //on own
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val r = parMap(as) {
      case a: A if f(a) => a :: Nil
      case default => List()
    }
    map(r)(_.flatten)
  }

  //from book
  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val filt = asyncF { (a: A) =>
      if (f(a)) List(a) else List()
    }
    val pars: List[Par[List[A]]] = l map filt
    map(sequence(pars))(_.flatten) // convenience method on `List` for concatenating a list of lists
  }

  //TODO: other things after 7.6

  //TODO: prove free theorem 7.7

  //TODO: test your fork law 7.8

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = p(e).get == p2(e).get

  def deadlock = {
    val a = lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    println(Par.equal(S)(a, fork(a)))
  }

  //TODO: show that any fixed sized thread pool will deadlock with first implementation of fork

}