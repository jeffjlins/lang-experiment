package fpis.ch8

import scala.util.{Failure, Success, Try}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(i: Int): Gen[A] = forSize(i)
  def map[B](f: A => B): SGen[B] = SGen { i => forSize(i).map(f) }
  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen {
      (i: Int) => forSize(i).flatMap((a: A) => f(a).apply(i))
    }
  }
}

case class Gen[A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    val g: A => State[RNG, B] = (a: A) => f(a).sample
    Gen(sample.flatMap(g))
  }

  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN2(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap { (i: Int) =>
      listOfN(i)
    }
  }

  def randomStream(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(sample.run(rng)))

  def unsized: SGen[A] = SGen(_ => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))
  }

  def unit[A](a: => A): Gen[A] = {
    Gen(State.unit(a))
  }

  val boolean: Gen[Boolean] = {
    Gen(State((rng: RNG) => RNG.boolean(rng)))
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen { (i: Int) =>
      g.listOfN(i)
    }
  }

  def listOfN1[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    def go(i: Int, s: State[RNG, A], l: List[State[RNG, A]]): List[State[RNG, A]] = {
      if (i > 0) s :: go(i - 1, s, l) else l
    }
    Gen(State.sequence(go(n, g.sample, Nil)))
  }

  def listOfN2[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def tuple(start: Int, stopExclusive: Int): Gen[(Int, Int)] = {
    Gen(choose(0, 5).sample.map2(choose(0, 5).sample)((_, _)))
  }

  def union1[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    val x = boolean.sample.flatMap { (b: Boolean) =>
      if (b) g1.sample else g2.sample
    }
    Gen(x)
  }

  def union2[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

//  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
//    RNG.
//  }

}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: String, successes: Int) extends Result {
  def isFalsified = true
}

object Prop {
  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (testCases: Int, rng: RNG) =>
    val counter = Stream.from(0)
    val rs = as.randomStream(rng).zip(counter).take(testCases).map { case (a, i) =>
      Try(f(a)) match {
        case Failure(e) => Falsified(buildMsg(a, e), i)
        case Success(r) if r => Passed
        case Success(r) if !r => Falsified(a.toString, i)
      }
    }
    rs.find(_.isFalsified).getOrElse(Passed)
  }

  private def buildMsg[A](s: A, e: Throwable): String = {
    s"""Test Case: $s
       |Exception: ${e.getMessage}
       |Stack Trace: ${e.getStackTrace.mkString("\n")}
     """.stripMargin
  }

}

// (test cases, rng)
case class Prop(run: (Int ,RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (i, rng) =>
    (run(i, rng), p.run(i, rng)) match {
      case (Passed, Passed) => Passed
      case (f: Falsified, Passed) => f
      case (Passed, f: Falsified) => f
      case (Falsified(m1, s1), Falsified(m2, s2)) => Falsified(s"$m1\n$m2", s1 + s2)
    }
  }

  def ||(p: Prop): Prop = Prop {  (i, rng) =>
    (run(i, rng), p.run(i, rng)) match {
      case (Passed, Passed) => Passed
      case (f: Falsified, Passed) => Passed
      case (Passed, f: Falsified) => Passed
      case (Falsified(m1, s1), Falsified(m2, s2)) => Falsified(s"$m1\n$m2", s1 + s2)
    }
  }
}

object Example {
  def main(args: Array[String]) {
    ex807()
  }

  def ex804() = {
    println(Gen.choose(5, 10).sample.run(RNG.Simple(63)))
  }

  def ex805() = {
    println(Gen.unit(5).sample.run(RNG.Simple(4)))
    println(Gen.boolean.sample.run(RNG.Simple(4)))
    println(Gen.listOfN2(3, Gen.choose(0, 5)).sample.run(RNG.Simple(4)))
    println(Gen.listOfN2(3, Gen.unit(6)).sample.run(RNG.Simple(4)))
  }

  def ex806() = {
    println(Gen.choose(0, 5).listOfN(Gen.unit(6)).sample.run(RNG.Simple(4)))
    println(Gen.unit(6).listOfN(Gen.choose(1, 5)).sample.run(RNG.Simple(4)))
  }

  def ex807() = {
    println(Gen.union2(Gen.choose(0, 9), Gen.choose(10, 19)).sample.run(RNG.Simple(27)))
  }

  //8.13 - skipping
  //8.14 - skipping
}