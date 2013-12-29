object ch11 {
  trait Functor[F[_]] {
    def map[A,B](fa: F[A])(f: A => B): F[B]

    def distribute[A,B](fab: F[(A,B)]): (F[A], F[B]) =
      (map(fab)(_._1), map(fab)(_._2))
  }

  val listFunctor = new Functor[List] {
    def map[A,B](fa: List[A])(f: A => B): List[B] = fa map f
  }

  import ch8._
  import Gen._
  import SGen._
  import Prop._
  def functorLaws[B, A[B]](F: Functor[A], genA: Gen[A[B]], genFB: Gen[B => B]) =
    forAll(genA) { a =>
      F.map(a)(identity) == a
    } &&
    forAll(genA ** genFB ** genFB) { case ((a, f), g) =>
      F.map(a)(f compose g) == F.map(F.map(a)(g))(f)
    }
  val listFunctorLaws = functorLaws(listFunctor, SGen.listOf(Gen.int), Gen.endoIntFunction)

  trait Monad[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => unit(f(a)))
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a,b)))

    // exercise 3
    def sequence[A](lma: List[F[A]]): F[List[A]] = {
      if (lma.isEmpty)
        unit(Nil)
      else {
        val head = lma.head
        val tail = lma.tail
        flatMap(head) { a =>
          map(sequence(tail)) (t => a :: t)
        }
      }
    }
    def traverse[A,B](lma: List[F[A]])(f: A => B): F[List[B]] = {
      if (lma.isEmpty)
        unit(Nil)
      else {
        val head = lma.head
        val tail = lma.tail
        flatMap(head) { a =>
          map(traverse(tail)(f)) (t => f(a) :: t)
        }
      }
    }

    // exercise 4
    // exercise 5: performs it n times
    def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
      if (n > 0) {
        flatMap(ma)(a => map(replicateM(n - 1, ma))(a :: _) )
      } else {
        unit(Nil)
      }
    }
    def replicateM_2[A](n: Int, ma: F[A]): F[List[A]] =
      sequence(List.fill(n)(ma))

    // exercise 6
    def filterM[A](lma: List[A])(f: A => F[Boolean]): F[List[A]] = {
      if (lma.isEmpty)
        unit(Nil)
      else {
        val head = lma.head
        val tail = lma.tail
        flatMap(f(head)) { bool =>
          if (bool)
            map(filterM(tail)(f))(head :: _)
          else
            filterM(tail)(f)
        }
      }
    }

    // exercise 9
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(b => g(b))

    // exercise 10
    def flatMap_2[A,B](fa: F[A])(f: A => F[B]): F[B] =
      compose[Unit, A, B](_ => fa, f)(())

    // exercise 12
    def rightIdentityLaw[A,B](fa: F[A])(f: A => F[B]): Boolean = 
      flatMap(flatMap(fa)(f))(b => unit(b)) == flatMap(fa)(f)
    def leftIdentityLaw[A,B](fa: F[A])(f: A => F[B]): Boolean = 
      flatMap(flatMap(fa)(a => unit(a)))(f) == flatMap(fa)(f)

    // exercise 13
    def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(identity)

    // exercise 14
    def flatMap_3[A,B](fa: F[A])(f: A => F[B]) = join(map(fa)(f))
  }

  object Monad {
    val genMonad = new Monad[Gen] {
      def unit[A](a: => A): Gen[A] = Gen.unit(a)
      def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
    }

    // exercise 1
    import ch7.Par
    val parMonad = new Monad[Par] {
      def unit[A](a: => A): Par[A] = Par.unit(a)
      def flatMap[A,B](fa: Par[A])(f: A => Par[B]): Par[B] = Par.flatMap(fa)(f)
    }
    val optionMonad = new Monad[Option] {
      def unit[A](a: => A): Option[A] = Some(a)
      def flatMap[A,B](fa: Option[A])(f: A => Option[B]): Option[B] =
        fa flatMap f
    }
    val listMonad = new Monad[List] {
      def unit[A](a: => A): List[A] = List(a)
      def flatMap[A,B](fa: List[A])(f: A => List[B]): List[B] =
        fa flatMap f
    }

    // exercise 2
    import ch6.State
    def stateMonad[S] = new Monad[({type l[A] = State[S,A]})#l] {
      def unit[A](a: => A): State[S,A] = State.unit(a)
      def flatMap[A,B](fa: State[S,A])(f: A => State[S,B]): State[S,B] =
        fa flatMap f
    }
  }

  // exercise 18
  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = Id(a)
    def flatMap[A,B](fa: Id[A])(f: A => Id[B]): Id[B] = fa flatMap f
  }

  import ch6._
  import State._
  val ISM = Monad.stateMonad[Int]
  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(ISM.unit(List.empty[(Int,A)]))((acc, a) =>
      for {
        n  <- get
        xs <- acc
        _  <- set(n + 1)
      } yield ((n,a) :: xs)
    ).run(0)._1.reverse

  // exercise 21
  case class Reader[R,A](run: R => A)
  object Reader {
    def readerMonad[R] = new Monad[({type l[A] = Reader[R,A]})#l] {
      def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
      def flatMap[A,B](ra: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
        Reader { r =>
          f(ra.run(r)).run(r)
        }
    }
  }
  case class Config(name: String, env: String, beta: Boolean)
  def makeConfigString = Reader[Config, String] { c =>
    s"${c.name} - ${c.env} (beta = ${c.beta})"
  }
  def makeConfigPage = Reader.readerMonad.map(makeConfigString)(str => s"<html>${str}</html>")
  def configStringAndPage = Reader.readerMonad.sequence(List(makeConfigString, makeConfigPage))
  def makeConfigPageMulitpleTimes =
    Reader.readerMonad.replicateM(5, makeConfigPage)
  val exampleConfig = Config(name = "Gib",
                             env = "Live",
                             beta = true)

}
