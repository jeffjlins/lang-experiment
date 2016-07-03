object Worksheet6a {
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
   object State {
      def unit[S, A](a: A): State[S, A] =
         State(s => (a, s))
      // The idiomatic solution is expressed via foldRight
      def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
         sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

      // This implementation uses a loop internally and is the same recursion
      // pattern as a left fold. It is quite common with left folds to build
      // up a list in reverse order, then reverse it at the end.
      // (We could also use a collection.mutable.ListBuffer internally.)
      def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
         def go(s: S, actions: List[State[S, A]], acc: List[A]): (List[A], S) =
            actions match {
               case Nil => (acc.reverse, s)
               case h :: t => h.run(s) match {
                  case (a, s2) => go(s2, t, a :: acc)
               }
            }
         State((s: S) => go(s, sas, List()))
      }

      // We can also write the loop using a left fold. This is tail recursive like the
      // previous solution, but it reverses the list _before_ folding it instead of after.
      // You might think that this is slower than the `foldRight` solution since it
      // walks over the list twice, but it's actually faster! The `foldRight` solution
      // technically has to also walk the list twice, since it has to unravel the call
      // stack, not being tail recursive. And the call stack will be as tall as the list
      // is long.
      def sequenceViaFoldLeft[S, A](l: List[State[S, A]]): State[S, List[A]] =
         l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)(_ :: _))

      def modifyOriginal[S](f: S => S): State[S, Unit] = for {
         s <- get // Gets the current state and assigns it to `s`.
         _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
//      } yield State.unit( () ) // is this the same?
      } yield () // why does this work? - it must find the unit method?

      def get[S]: State[S, S] = State(s => (s, s))

      def set[S](s: S): State[S, Unit] = State(_ => ((), s))

      def modifyNew[S](f: S => S): State[S, Unit] = {
         get.flatMap { s =>
            set(f(s))
         }
      }

      def modifyNewer[S](f: S => S): State[S, Unit] = {
         get.flatMap { s =>
            set(f(s)).map { i =>
               ()
            }
         }
      }

      def modifyNewerer[S](f: S => S): State[S, Unit] = {
         val sf = State((s: S) => (s, s))
         sf.flatMap { s => // i.e. input to flatmap is X to State
            val res = f(s)
            State[S, Unit] { _ =>
               ((), res)
            }
//            val rf2 = State(_ => ((), res))
//            val rf = State[S, Unit] { _ =>
//               ((), res)
//            }
//            rf.map { i =>
//               ()
//            }
         }
      }

      def modify[S](f: S => S): State[S, Unit] = {
         State{ s =>
            ((), f(s))
         }
      }
   }

   sealed trait Input

   case object Coin extends Input

   case object Turn extends Input

   case class Machine(locked: Boolean, candies: Int, coins: Int)

   object Candy {
      def update(i: Input) = {
         (s: Machine) => (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) =>
               Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
               Machine(true, candy - 1, coin)
         }
      }

      def updateOriginal = (i: Input) => (s: Machine) =>
         (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) =>
               Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
               Machine(true, candy - 1, coin)
         }

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
         val one = inputs.map { (inp) =>
               val first = update(inp) // returns function that goes from Machine to Machine using the input provided here; so it has the input already but it has not executed
               State.modify[Machine](first)
         }
         val two = State.sequence(one).flatMap { listOfUnitToStateFunction => // don't care about unit input, just execute
            State.get.map { s => // input is machine
               (s.candies, s.coins)
            }
         }
         two
      }

      def simulateMachineOriginal(inputs: List[Input]): State[Machine, (Int, Int)] = for {
         _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
         s <- State.get
      } yield (s.candies, s.coins)
   }

   //# last one
   val actions = Coin :: Turn :: Coin :: Coin :: Nil
   val simulation: State[Machine, (Int, Int)] = Candy.simulateMachine(actions)
   val initialState = Machine(locked = true, candies = 3, coins = 2)
   simulation.run(initialState)
   // should return 2, 4

   State.modify((i: Int) => i + 1).run(6)

   State.modifyNew((i: Int) => i + 1).run(6)

   State.modifyNewer((i: Int) => i + 1).run(6)

   State.modifyNewerer((i: Int) => i + 1).run(6)

   State.modifyOriginal((i: Int) => i + 1).run(6)

//   val rf = State(_ => ((), res))

   //=======================
   //   (Input, Machine) => Machine                 map()...update()
   //   Machine => Machine                          update()
   //   Machine => (Machine, Unit)                  modify()
   //   Machine => (candies, coins)                 flatMap.map

   //   List(Input)
   //   List( [closure: Input =>] Machine => Machine )
   //   List( [closure: Input =>] Machine => (Machine, Unit) )
   //   List( [closure: Input =>] Machine => (Machine.candies, Machine.coins) )


}