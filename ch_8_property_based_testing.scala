object ch8 {
  // exercise 1
  // forAll(intList) { l => l.reverse.sum == l.sum } &&
  // forAll(intList, intList) { (l1, l2) => (l1 ++ l2).sum == l1.sum + l2.sum } &&
  // forAll(int, int) { (i, size) => List.fill(size)(i).sum == (i * sum) }

  // exercise 2
  // property("max") = {
  //   forAll(intList) { l => l.reverse.max == l.max } &&
  //   forall(int, nonNegativeInt) { i, size => List.fill(size)(i).max == i } &&
  //   forAll(intList, intList) { l1, l2 => (l1 ++ l2).max == (l1.max, l2.max).max }
  // }

  import ch6.{Rand, RNG, State}

  case class Gen[+A](sample: State[RNG,A]) { self =>
    // exercise 6
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen {
      sample.flatMap(a => f(a).sample)
    }
    def listOfN(size: Gen[Int]): Gen[List[A]] =
      size.flatMap(n => Gen.listOfN(n, this))

    def map[B](f: A => B): Gen[B] =
      flatMap(a => Gen.unit(f(a)))

    def map2[B,C](gb: Gen[B])(f: (A,B) => C): Gen[C] =
      for {
        a <- this
        b <- gb
      } yield f(a,b)

    def **[B](gb: Gen[B]): Gen[(A,B)] =
      map2(gb)((_,_))

    // exercise 10
    def unsized: SGen[A] = SGen { i =>
      self
    }
  }

  object Gen {
    // exercise 4
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen {
        State {
          Rand.map(RNG.positiveInt) { i =>
            (i % (stopExclusive - start)) + start
          }
        }
      }

    // exercise 5
    def unit[A](a: => A): Gen[A] = Gen {
      State.unit(a)
    }
    def boolean: Gen[Boolean] = Gen {
      choose(0,2).sample.map(i => i % 2 == 0)
    }
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen {
      val samples = List.fill(n)(g.sample)
      State.sequence(samples)
    }

    // exercise 6
    def sameParity(from: Int, to: Int): Gen[(Int, Int)] =
      for {
        i <- choose(from,to)
        j <- choose(from,to)
      } yield (i,j)

    // exercise 7
    def union[A](g1: => Gen[A], g2: => Gen[A]): Gen[A] = for {
      choose <- Gen.boolean
      a      <- if (choose) g1 else g2
    } yield a

    // exercise 8
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      val (gen1, r1) = g1
      val (gen2, r2) = g2
      val total = r1 + r2
      for {
        i      <- choose(0, 1000)
        ratio  = (i / 1000.0) * total
        choose = (ratio <= r1)
        a      <- if (choose) gen1 else gen2
      } yield a
    }

    def int: Gen[Int] = Gen.choose(Int.MinValue, Int.MaxValue)
  }

  case class SGen[+A](forSize: Int => Gen[A]) { self =>
    // exercise 11
    def flatMap[B](f: A => SGen[B]): SGen[B] = SGen { i =>
      forSize(i).flatMap { a =>
        f(a).forSize(i)
      }
    }
    def map[B](f: A => B): SGen[B] =
      flatMap(a => SGen.unit(f(a)))
  }

  object SGen {
    def unit[A](a: => A): SGen[A] = SGen(_ => Gen.unit(a))

    // exercise 12
    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen { i => Gen.listOfN(i, g) }

    // exercise 16
    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen { i => Gen.listOfN(i max 1, g)}
  }

  type FailedCase   = String
  type SuccessCount = Int
  type MaxSize      = Int
  type TestCases    = Int
  type Result       = Option[(FailedCase, SuccessCount)]

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) { self =>

    // exercise 9
    def &&(p: Prop): Prop = Prop { (max, i, rng) =>
      self.run(max, i,rng) orElse p.run(max, i,rng)
    }
    def ||(p: Prop): Prop = Prop { (max, i, rng) =>
      val result = self.run(max, i,rng)
      if (result.isDefined) {
        val pResult = p.run(max, i, rng)
        if (pResult.isDefined) {
          // should make a better message, but oh well
          result
        } else None
      } else None
    }
  }

  object Prop {
    import ch5.Stream

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (_, n, rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n) map {
        case (a, i) => try {
          if (f(a)) None else Some((a.toString, i)) 
        } catch { case e: Exception => Some((buildMsg(a,e), i)) }
      } find (_.isDefined) getOrElse None
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
      forAll(g.forSize(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
                    p.run(max, casesPerSize, rng)
                  }).toList.reduceLeft(_ && _)
      prop.run(max, n, rng)
    }

    def check(p: => Boolean): Prop =
      forAll(Gen.unit(()))(_ => p)

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = RNG.simple(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Some((msg, n)) =>
          println(s"! Falsified after $n passed tests: \n $msg")
        case None =>
          println(s"+ OK, passed $testCases tests.")
      }

    private[this] def randomStream[A](as: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng) { rng =>
        val (a, next) = as.sample.run(rng)
        Some(a, next)
      }

    private[this] def buildMsg[A](s: A, e: Exception): String =
      s"test case $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stacktrace: \n ${e.getStackTrace.mkString("\n")}"
  }

  import Gen._
  import SGen._
  import Prop._

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { l =>
    val max = l.max
    ! l.exists(_ > max)
  }

  // exercise 16
  val sortedProp = forAll(listOf(smallInt)) { l =>
    @annotation.tailrec
    def headIsLessThanAllInTail(l: List[Int]): Boolean = l match {
      case head :: tail => tail.forall(head <= _) && headIsLessThanAllInTail(tail)
      case Nil          => true
    }
    headIsLessThanAllInTail(l.sorted)
  }

  object ParProperties {
    import ch7._
    import java.util.concurrent._
    val ES = Executors.newCachedThreadPool

    val p1 = Prop.forAll(Gen.unit(Par.unit(1))) { i =>
      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get
    }

    val p2 = Prop.check {
      val p = Par.map(Par.unit(1))(_ + 1)
      val p2 = Par.unit(2)
      p(ES).get == p2(ES).get
    }

    def equal[A](p1: Par[A], p2: Par[A]): Par[Boolean] =
      Par.map2(p1,p2)(_ == _)

    val p3 = Prop.check {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      ) (ES) get
    }

    import Gen._

    val S = weighted(
      choose(1,4).map(Executors.newFixedThreadPool) -> 0.75,
      unit(Executors.newCachedThreadPool) -> 0.25
    )

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      Prop.forAll(S ** g) {
        case (s,a) => f(a)(s).get
      }

    def checkPar(p: Par[Boolean]): Prop =
      forAllPar(Gen.unit(()))(_ => p)

    def p3_2 = checkPar {
      equal(
        Par.map(Par.unit(1))(_ + 1),
        Par.unit(2)
      )
    }

    val pint = Gen.choose(0,10) map (Par.unit)
    val p4 =
      forAllPar(pint)(n => equal(Par.map(n)(identity), n))

    // exercise 17
    val pintComplex: Gen[Par[Int]] = {
      lazy val recursiveAdd = (pintComplex ** pintComplex) map {
        case (p1,p2) => Par.map2(p1,p2)(_ + _)
      }
      Gen.union(pint, recursiveAdd)
    }
    val p4_2 =
      forAllPar(pintComplex)(n => equal(Par.map(n)(identity), n))

    // exercise 18
    val forkProp =
      forAllPar(pint)(n => equal(Par.fork(n), n))
  }

  import Gen._
  // exercise 19
  // val takeWhileProp2 =
  //   forAll(Gen.listOf(int) ** Gen.function[Int, Boolean]) {
  //     case (is, f) =>
  //       val result = is.takeWhile(f)
  //       val rest = is.drop(result.size)
  //       rest match {
  //         case head :: _ => f(head) == false
  //         case Nil       => true
  //       }
  // }
  // val takeWhileProp3 =
  //   forAll(Gen.listOf(int) ** Gen.function[Int, Boolean]) {
  //     case (is, f) =>
  //       is.takeWhile(f) ++ is.dropWhile(f) == is
  //   }

  val isEven = (i: Int) => i % 2 == 0
  val takeWhileProp =
    Prop.forAll(SGen.listOf(int))(l => l.takeWhile(isEven).forall(isEven))

}
