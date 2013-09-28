object ch6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  object RNG {
    def simple(seed: Long): RNG = new RNG {
      def nextInt: (Int, RNG) = {
        val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int],
         simple(seed2))
      }
    }

    lazy val test = simple(-123)

    def randomPair(rng: RNG): ((Int, Int), RNG)  = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    // exercise 1
    def positiveInt(rng: RNG): (Int, RNG) = {
      val (i, rng2) = rng.nextInt
      if (i == Int.MinValue)
        (Int.MaxValue, rng2)
      else
        (i.abs, rng2)
    }

    // exercise 2
    def double(rng: RNG): (Double, RNG) = {
      val (i, rng2) = positiveInt(rng)
      (i.toFloat / Int.MaxValue, rng2)
    }

    // exercise 3
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, rng2) = rng.nextInt
      val (d, rng3) = double(rng2)
      ((i,d), rng3)
    }
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val (d, rng2) = double(rng)
      val (i, rng3) = rng2.nextInt
      ((d,i), rng3)
    }
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, rng2) = double(rng)
      val (d2, rng3) = double(rng2)
      val (d3, rng4) = double(rng3)
      ((d1,d2,d3), rng4)
    }

    // exercise 4
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      @annotation.tailrec
      def go(count: Int, rng: RNG, soFar: List[Int]): (List[Int], RNG) =
        if (count > 0) {
          val (i, rng2) = rng.nextInt
          go(count - 1, rng2, i :: soFar)
        } else {
          (soFar, rng)
        }

      go(count, rng, Nil)
    }
  }

  object Rand {
    type Rand[+A] = RNG => (A, RNG)

    import RNG._

    val int: Rand[Int] = _.nextInt

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def positiveEven: Rand[Int] =
      map(positiveInt)(i => i - (i % 2))

    // exercise 5
    def double_2: Rand[Double] =
      map(positiveInt)(i => i.toFloat / Int.MaxValue)

    // exercise 6
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
      rng => {
        val (a, rng2) = ra(rng)
        val (b, rng3) = rb(rng2)
        (f(a,b), rng3)
      }

    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
      map2(ra, rb)(Pair.apply)

    val randIntDouble: Rand[(Int, Double)] =
      both(int, double)
    val randDoubleInt: Rand[(Double, Int)] =
      both(double, int)

    // exercise 7
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      rng => {
        fs.foldLeft((List.empty[A], rng)) { (asRng, ra) =>
          val (as, rng) = asRng
          val (a, nextRng) = ra(rng)
          (as :+ a, nextRng)
        }
      }
    def ints_2(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    def positiveLessThan(n: Int): Rand[Int] =
      map(positiveInt)(_ % n)

    // exercise 8
    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, rng2) = f(rng)
        g(a)(rng2)
      }

    def positiveLessThan_2(n: Int): Rand[Int] =
      flatMap(positiveInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod > 0)
          unit(mod)
        else
          positiveLessThan_2(n)
      }

    // exercise 9
    def map_2[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))
    def map2_2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
      flatMap(ra) { a =>
        flatMap(rb) { b =>
          unit(f(a,b))
        }
      }

    def rollDie: Rand[Int] = map(positiveLessThan(6)) { _  + 1 }
  }

  type State_as_func[S, +A] = S => (A,S)
  type Rand_2[A] = State_as_func[RNG, A]

  // exercise 10
  case class State[S, +A](run: S => (A,S)) {
    def map[B](f: A => B): State[S, B] =
      State { s =>
        val (a, s2) = run(s)
        (f(a), s2)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, s2) = run(s)
        f(a).run(s2)
      }
  }

  object State {
    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    def map2[S, A, B, C](sa: State[S, A],
                         sb: State[S, B])(f: (A,B) => C): State[S, C] =
      for {
        a <- sa
        b <- sb
      } yield f(a,b)

    def sequence[S, A](sas: List[State[S,A]]): State[S, List[A]] =
      State { s =>
        sas.foldLeft(List.empty[A], s) { (asS, sa) =>
          val (as, s) = asS
          val (a, nextS) = sa.run(s)
          (as :+ a, nextS)
        }
      }

    def get[S]: State[S, S] = State(s => (s,s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  type Rand[A] = State[RNG, A]

  // exercise
  object CandyDispenser {
    sealed trait Input
    case object Coin extends Input
    case object Turn extends Input

    case class Machine(locked: Boolean, candies: Int, coins: Int) {
      def withInput(input: Input): Machine = input match {
        case _ if candies <= 0 => this
        case Coin if locked    => copy(locked = false, coins = coins + 1)
        case Turn if !locked   => copy(locked = true, candies = candies - 1)
        case _                 => this
      }
    }

    object Machine {
      def test = Machine(true, 5, 10)
    }

    import State._

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      val actions: List[State[Machine, Unit]] =
        inputs.map(i => State((m: Machine) => ((), m.withInput(i))))
      for {
        _ <- sequence(actions)
        m <- get
      } yield (m.coins, m.candies)
    }

    val testInputs =
      List(
        Coin, Turn, // dispense one candy
        Turn, // shouldn't do anything
        Coin, Turn, // another candie
        Coin, Turn, // ...
        Coin, Turn // ...
      )
    
  }
  
}
