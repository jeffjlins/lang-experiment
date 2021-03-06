import scala.util.Try

//import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

object Worksheet {

   //# 4.1
   sealed trait Option[+A] {
      def map[B](f: A => B): Option[B] = this match {
         case Some(x) => Some(f(x))
         case None => None
      }

      def getOrElse[B >: A](default: => B): B = this match {
         case Some(x) => x
         case None => default
      }

      def flatMap[B](f: A => Option[B]): Option[B] = this match {
         case Some(x) => f(x)
         case None => None
      }

      def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
         case Some(_) => this
         case None => ob
      }

      def filter(f: A => Boolean): Option[A] = this match {
         case Some(x) => if (f(x)) this else None
         case None => None
      }
   }
   case class Some[+A](get: A) extends Option[A]
   case object None extends Option[Nothing]

   object Option {
      def failingFn(i: Int): Int = {
         val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
         try {
            val x = 42 + 5
            x + y
         }
         catch {
            case e: Exception => 43
         } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
      }

      def failingFn2(i: Int): Int = {
         try {
            val x = 42 + 5
            x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
         }
         catch {
            case e: Exception => 43
         }
      }

      def mean(xs: Seq[Double]): Option[Double] = {
         if (xs.isEmpty) None
         else Some(xs.sum / xs.length)
      }

      //# 4.2
      def variance(xs: Seq[Double]): Option[Double] = {
         mean(xs).flatMap { m =>
            mean(xs.map(x => math.pow(x - m, 2)))
         }
      }
      def lessNaiveVariance(xs: Seq[Double]): Option[Double] = {
         mean(xs).map { m =>
            val vr = xs.map(x => math.pow(x - m, 2))
            vr.sum / vr.length
         }
      }
      def naiveVariance(xs: Seq[Double]): Option[Double] = {
         val m: Double = mean(xs).getOrElse(0)
         val vr = xs.map(x => math.pow(x - m, 2))
         Some(vr.sum / vr.length)
      }

      //# 4.3
      def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
         for (aa <- a; bb <- b) yield f(aa, bb)
      }
      def lessNaiveMap2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
         a flatMap { aa =>
            b map { bb =>
               f(aa, bb)
            }
         }
      }
      def naiveMap2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
         case (None, _) => None
         case (_, None) => None
         case (Some(l), Some(r)) => Some(f(l, r))
      }

      //# 4.4
      def sequence[A](a: List[Option[A]]): Option[List[A]] = {
         a.foldRight[Option[List[A]]](Some(Nil)) { (el, acc) =>
            for (eli <- el; acci <- acc) yield eli :: acci
         }
      }
      def sequenceLessNaive[A](a: List[Option[A]]): Option[List[A]] = {
         a.foldRight[Option[List[A]]](Some(Nil)) { (el, acc) =>
            el.flatMap { eli =>
               acc.map { acci =>
                  eli :: acci
               }
            }
         }
      }
      def sequenceNaive[A](a: List[Option[A]]): Option[List[A]] = a match {
         case Nil => Some(Nil)
         case h :: t => h flatMap { x =>
            sequence(t) map { y =>
               x :: y
            }
         }
      }

      //# 4.5
      def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
         a.foldRight[Option[List[A]]](Some(Nil)) { (el, acc) =>
            for (eli <- el; acci <- acc) yield eli :: acci
         }
      }
      def traverseNaive[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
         sequence(a map f)
      }
   }
   //# 4.1
   Some(3).map(_ + 2)
   None.map((x: Int) => x + 2)
   Some(3).getOrElse(99)
   None.getOrElse(99)
   Some(3).flatMap(x => if ((x - 2) == 0) None else Some(x - 2))
   Some(2).flatMap(x => if ((x - 2) == 0) None else Some(x - 2))
   None.flatMap((x: Int) => if ((x - 2) == 0) None else Some(x - 2))

   Some(3).orElse(Some(5))
   None.orElse(Some(5))

   Some(3).filter(_ < 3)
   Some(2).filter(_ < 3)
   //# 4.2
   val wik: Seq[Double] = Seq(3, 4, 7, 10)
   Option.naiveVariance(wik)
   Option.naiveVariance(Nil: Seq[Double])
   Option.lessNaiveVariance(wik)
   Option.lessNaiveVariance(Nil: Seq[Double])
   Option.variance(wik)
   Option.variance(Nil: Seq[Double])
   //# 4.3
   Option.map2(Some(3), Some(4))(_ + _)
   Option.map2(Some(3), None: Option[Int])(_ + _)
   //# 4.4
   val ol = List(Some(1), Some(3), Some(6))
   val ol2 = List(Some(1), None, Some(3), Some(6))
   Option.sequence(ol)
   Option.sequence(ol2)

   //# 4.5
   Option.traverse(List(1, 2))(i => if (i > 2) None else Some(i))
   Option.traverse(List(1, 2, 3))(i => if (i > 2) None else Some(i))
}