object Worksheet6 {

   trait RNG {
      def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
   }

   object RNG {
      // NB - this was called SimpleRNG in the book text

      case class Simple(seed: Long) extends RNG {
         def nextInt: (Int, RNG) = {
            val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
            val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
            val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
            (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
         }
      }

      //# 6.1
      def nonNegativeInt(rng: RNG): (Int, RNG) = {
         rng.nextInt match {
            case (Int.MinValue, rng2) => (0, rng2)
            case (x, rng2) if x < 0 => (x * -1, rng2)
            case x => x
         }
      }

      //# 6.2
      def double(rng: RNG): (Double, RNG) = {
         val (i, rng2) = rng.nextInt
         (i.toDouble / (Int.MaxValue + Int.MinValue).toDouble, rng2)
      }

      //# 6.3
      def intDouble(rng: RNG): ((Int, Double), RNG) = {
         val (i, rng2) = rng.nextInt
         val (d, rng3) = double(rng)
         ((i, d), rng3)
      }

      def doubleInt(rng: RNG): ((Double, Int), RNG) = {
         val ((i, d), rng2) = intDouble(rng)
         ((d, i), rng2)
      }

      def double3(rng: RNG): ((Double, Double, Double), RNG) = {
         val (d1, rng2) = double(rng)
         val (d2, rng3) = double(rng2)
         val (d3, rng4) = double(rng3)
         ((d1, d2, d3), rng4)
      }

      //# 6.4
      def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
         case 0 => (Nil, rng)
         case 1 => val (i, rng2) = rng.nextInt; (i :: Nil, rng2)
         case c => val (i, rng3) = rng.nextInt; val (l, rng4) = ints(c - 1)(rng3); (i :: l, rng4)
      }

      type Rand[+A] = RNG => (A, RNG)

      val int: Rand[Int] = _.nextInt

      def unit[A](a: A): Rand[A] = {
         rng => (a, rng)
      }

      def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
         rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
         }
      }

      //# 6.5
      def double2(rng: RNG): (Double, RNG) = {
         map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1).toDouble)(rng)
      }

      //# 6.6
      def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
         rng =>
            val (a, rng2) = ra(rng)
            val (b, rng3) = rb(rng2)
            (f(a, b), rng3)
      }

      def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
      val randIntDouble: Rand[(Int, Double)] = both(int, double)
      val randDoubleInt: Rand[(Double, Int)] = both(double, int)

      //# 6.7
      def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
         case h :: Nil => map(h)(_ :: Nil)
         case h :: t => map2(h, sequence(t))(_ :: _)
      }

      def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
         fs.foldRight(unit(Nil:List[A]))((a, b) => map2(a, b)(_ :: _))
      }

      def ints2(count: Int): Rand[List[Int]] = {
         sequence(List.fill(count)(int))
      }

      def ints3(count: Int): Rand[List[Int]] = {
         sequence2(List.fill(count)(int))
      }

      //# 6.8
      def nonNegativeLessThan(n: Int): Rand[Int] = {
         map(nonNegativeInt) {_ % n}
      }

//      def nonNegativeLessThan2(n: Int): Rand[Int] = {
//         map(nonNegativeInt) { i =>
//            val mod = i % n
//            if (i + (n - 1) - mod >= 0) mod
//            else nonNegativeLessThan2(n)(???)
//         }
//      }

      def nonNegativeLessThan2(n: Int): Rand[Int] = {
         rng =>
            val (i, rng2) = nonNegativeInt(rng)
            val mod = i % n
            if (i + (n - 1) - mod >= 0)
               (mod, rng2)
            else nonNegativeLessThan2(n)(rng2)
      }

      def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
         rng =>
            val (i, rng2) = f(rng)
            g(i)(rng2)
      }

      def nonNegativeLessThan3(n: Int): Rand[Int] = {
         flatMap(nonNegativeInt) { i =>
            rng =>
               val mod = i % n
               if (i + (n - 1) - mod >= 0) (mod, rng)
               else nonNegativeLessThan3(n)(rng)
         }
      }

      def nonNegativeLessThan4(n: Int): Rand[Int] = {
         flatMap(nonNegativeInt) { i =>
            val mod = i % n
            if (i + (n - 1) - mod >= 0) unit(mod)
            else nonNegativeLessThan4(n)
         }
      }

      //# 6.8
      def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
         flatMap(s)(a =>
            rng2 => (f(a), rng2)
         )
      }

      def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
         flatMap(ra)(a =>
            mapViaFlatMap(rb)(b =>
               f(a, b)
            )
         )
      }

      def both2[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2ViaFlatMap(ra, rb)((_, _))
      val randIntDouble2: Rand[(Int, Double)] = both(int, double)
      val randDoubleInt2: Rand[(Double, Int)] = both(double, int)

      // the for comprehension doesn't work because flatMap/map is not a member of Rand
//      def map2ViaFlatMap2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//         for (a <- ra; b <- rb) yield f(a, b)
//      }
//      def both3[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2ViaFlatMap2(ra, rb)((_, _))
//      val randIntDouble3: Rand[(Int, Double)] = both(int, double)
//      val randDoubleInt3: Rand[(Double, Int)] = both(double, int)
   }

   //# 6.9
   case class State[S, +A](run: S => (A, S)) {
      def map[B](f: A => B): State[S, B] = {
         val rf = (rng: S) => {
            val (a, rng2) = run(rng)
            (f(a), rng2)
         }
         State(rf)
      }

      def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = {
         val rf = (rng: S) => {
            val (a, rng2) = run(rng)
            val (b, rng3) = sb.run(rng2)
            (f(a, b), rng3)
         }
         State(rf)
      }

      def flatMap[B](f: A => State[S, B]): State[S, B] = {
         val rf = (rng: S) => {
            val (a, rng2) = run(rng)
            f(a).run(rng2)
         }
         State(rf)
      }
   }

//   sealed trait Input
//   case object Coin extends Input
//   case object Turn extends Input
//   case class Machine(locked: Boolean, candies: Int, coins: Int)
//   object State {
//      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
//         val nstate = inputs.head match {
//            case Coin => simulateMachine(inputs.tail).map()
//         }
//      }
//   }



   //=======================
   //# 6.1
   RNG.Simple(1).nextInt
   RNG.Simple(-1).nextInt
   RNG.nonNegativeInt(RNG.Simple(1))
   RNG.nonNegativeInt(RNG.Simple(-1))
   //# 6.1
   RNG.Simple(1).nextInt
   RNG.Simple(-1).nextInt
   RNG.double(RNG.Simple(1))
   RNG.double(RNG.Simple(-1))
   //# 6.3
   RNG.intDouble(RNG.Simple(1))
   RNG.doubleInt(RNG.Simple(1))
   RNG.double3(RNG.Simple(1))
   //# 6.4
   RNG.ints(3)(RNG.Simple(1))
   RNG.nonNegativeInt(RNG.Simple(1))
   RNG.map(RNG.nonNegativeInt)(_ + 1)(RNG.Simple(1))
   //# 6.5
   RNG.double2(RNG.Simple(1))
   //# 6.5
   RNG.randDoubleInt(RNG.Simple(1))
   RNG.randIntDouble(RNG.Simple(1))
   //# 6.7
   RNG.ints(3)(RNG.Simple(1))
   RNG.ints2(3)(RNG.Simple(1))
   //# 6.8
   RNG.nonNegativeLessThan(10000)(RNG.Simple(1))
   RNG.nonNegativeLessThan2(10000)(RNG.Simple(1))
   RNG.nonNegativeLessThan3(10000)(RNG.Simple(1))
   RNG.nonNegativeLessThan4(10000)(RNG.Simple(1))
   //# 6.9
   RNG.mapViaFlatMap(RNG.nonNegativeInt)(_ + 1)(RNG.Simple(1))
   RNG.randDoubleInt2(RNG.Simple(1))
   RNG.randIntDouble2(RNG.Simple(1))
//   RNG.randDoubleInt3(RNG.Simple(1))
//   RNG.randIntDouble3(RNG.Simple(1))


   //# last one
   def addUmm(x: String): String = x + " umm"
   def addAhem(x: String): String = x + " ahem"
   val ummThenAhem = (addAhem _).compose(addUmm _)
   ummThenAhem("start")



   object State {
      type Rand[A] = State[RNG, A]

      def unit[S, A](a: A): State[S, A] =
         State(s => (a, s))

      // The idiomatic solution is expressed via foldRight
      def sequenceViaFoldRight[S,A](sas: List[State[S, A]]): State[S, List[A]] =
         sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

      // This implementation uses a loop internally and is the same recursion
      // pattern as a left fold. It is quite common with left folds to build
      // up a list in reverse order, then reverse it at the end.
      // (We could also use a collection.mutable.ListBuffer internally.)
      def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
         def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
            actions match {
               case Nil => (acc.reverse,s)
               case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
            }
         State((s: S) => go(s,sas,List()))
      }

      // We can also write the loop using a left fold. This is tail recursive like the
      // previous solution, but it reverses the list _before_ folding it instead of after.
      // You might think that this is slower than the `foldRight` solution since it
      // walks over the list twice, but it's actually faster! The `foldRight` solution
      // technically has to also walk the list twice, since it has to unravel the call
      // stack, not being tail recursive. And the call stack will be as tall as the list
      // is long.
      def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
         l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))

      def modify[S](f: S => S): State[S, Unit] = for {
         s <- get // Gets the current state and assigns it to `s`.
         _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
      } yield ()

      def get[S]: State[S, S] = State(s => (s, s))

      def set[S](s: S): State[S, Unit] = State(_ => ((), s))
   }

   sealed trait Input
   case object Coin extends Input
   case object Turn extends Input
   case class Machine(locked: Boolean, candies: Int, coins: Int)
   object Candy {
      def update = (i: Input) => (s: Machine) =>
         (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) =>
               Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
               Machine(true, candy - 1, coin)
         }

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
         _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
         s <- State.get
      } yield (s.coins, s.candies)
   }

   //# last one

   val actions = Coin :: Turn :: Coin :: Coin :: Nil
   val simulation: State[Machine, (Int, Int)] = Candy.simulateMachine(actions)
   val initialState = Machine(locked=true, candies=3, coins=2)
   simulation.run(initialState)


}