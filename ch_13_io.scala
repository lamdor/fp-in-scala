object ch13 {
  // case class Player(name: String, score: Int)

  // def winnerMsg(p: Player): String =
  //   s"${p.name} is the winner!"

  // def printWinner(p: Player): IO =
  //   PrintLine(winnerMsg(p))

  // def winner(p1: Player, p2: Player): Player =
  //   if (p1.score > p2.score) p1
  //   else p2

  // def declareWinner(p1: Player, p2: Player): Unit =
  //   printWinner(winner(p1, p2))

  trait IO[+A] { self =>
    def run: A

    def map[B](f: A => B): IO[B] =
      new IO[B] { def run = f(self.run) }

    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] { def run = f(self.run).run }
  }

  import ch12.Monad

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f
    def apply[A](a: => A): IO[A] = unit(a)
  }


  def ReadLine: IO[String] = IO { readLine }
  def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0 / 9.0

  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temp in degrees fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  object continuations {
    trait IO[F[_], +A]
    case class Pure[F[_], +A](get: A) extends IO[F,A]
    case class Request[F[_], I, +A](
      expr: F[I],
      receive: I => IO[F,A]
    ) extends IO[F,A] {
      def andThen[B](f: IO[F,A] => IO[F,B]): Request[F, I, B] =
        Request(expr, (i: I) => f(receive(i)))
    }

    trait Console[A]
    case object ReadLine extends Console[Option[String]]
    case class PrintLine(s: String) extends Console[Unit]

    trait Run[F[_]] {
      def apply[A](expr: F[A]): (A, Run[F])
    }

    object IO {
      @annotation.tailrec
      def run[F[_], A](R: Run[F])(io: IO[F,A]): A = io match {
        case Pure(a: F[A]) => R(a)._1
        case Request(expr, recv) =>
          R(expr) match { case (e, r2) => run(r2)(recv(e)) }
      }
    }

    object RunConsoleMock extends Run[Console] {
      def apply[A](c: Console[A]) = c match {
        case ReadLine => (Some("Hello world!"), RunConsoleMock)
        case PrintLine(_) => ((), RunConsoleMock)
      }
    }

    object RunConsole extends Run[Console] {
      def apply[A](c: Console[A]) = c match {
        case ReadLine =>
          val r = try Some(readLine) catch { case _: Throwable => None }
          (r, RunConsole)
        case PrintLine(s) => (println(s), RunConsole)
      }
    }

    // exercise 1
    def monad[F[_]] = new Monad[({type l[x] = IO[F,x]})#l] {
      def unit[A](a: => A): IO[F,A] = Pure(a)
      def flatMap[A, B](io: IO[F,A])(f: A => IO[F,B]): IO[F,B] = io match {
        case Pure(a) => f(a)
        case r: Request[F, _, A] => r andThen (flatMap(_)(f))
      }
    }

    // exercise 2
    def console(lines: List[String]): Run[Console] = new Run[Console] {
      def apply[A](c: Console[A]) = c match {
        case ReadLine     => (lines.headOption, console(lines.tail))
        case PrintLine(_) => ((),               console(lines))
      }
    }

    // exercise 3
    def run[F[_], A](F: Monad[F])(io: IO[F,A]): F[A] = io match {
      case Pure(a) => F.unit(a)
      case Request(expr, recv) =>
        val next: F[IO[F,A]] = F.map(expr)(recv)
        F.flatMap(next)(run(F))
    }

    def converter: IO[Console, _] = { 
      val F = monad[Console]
      val readDouble: IO[Console, Double] =
        Request(ReadLine, (os: Option[String]) => F.unit(os.map(_.toDouble).getOrElse(0)))
      F.sequence(
        List(
          F.unit(PrintLine("Enter a temp in degrees fahrenheit: ")),
          F.map(readDouble)(f =>
            PrintLine(fahrenheitToCelsius(f).toString)
          )
        )
      )
    }
  }

  trait Trampoline[+A]
  case class Done[+A](get: A) extends Trampoline[A]
  case class More[+A](force: () => Trampoline[A]) extends Trampoline[A]
  case class Bind[A, +B](force: () => Trampoline[A],
                         f: A => Trampoline[B]) extends Trampoline[B]

  // exercise 5
  @annotation.tailrec
  def run[A](t: Trampoline[A]): A = t match {
    case Done(get) => get
    case More(force) => run(force.apply)
    case Bind(force, f) =>
      force.apply match {
        case Done(get)      => run(f(get))
        case More(force)    => run(Bind(force, f))
        case Bind(force, g) => run(Bind(force, f compose g))
      }
  }

  val exampleTrampoline: Trampoline[Int] =
    Bind(() => More(() => Done(1)),
         (i: Int) => Bind(() => Done(1),
                          (j: Int) => More(() => Done(i + j + 1))))

  // exercise 6
  val trampolineMonad = new Monad[Trampoline] {
    def unit[A](a: => A): Trampoline[A] = More(() => Done(a))
    def flatMap[A, B](fa: Trampoline[A])(f: A => Trampoline[B]): Trampoline[B] =
      Bind(() => fa, f)
  }

  // extra credit?
  // from http://blog.richdougherty.com/2009/04/tail-calls-tailrec-and-trampolines.html
  // applied to trampolines
  def odd(n: Int): Boolean =
    if (n <= 0) false
    else even(n - 1)
  def even(n: Int): Boolean =
    if (n <= 0) true
    else odd(n - 1)

  // even(9999) results in java.lang.StackOverflowError
  // so let's jump on the trampoline
  def oddTrampoline(n: Int): Trampoline[Boolean] = {
    val nt = Done(n)
    trampolineMonad.flatMap(nt) { i => 
      if (i <= 0) Done(false)
      else Bind(() => Done(i - 1), evenTrampoline)
    }
  }
  def evenTrampoline(n: Int): Trampoline[Boolean] = {
    val nt = Done(n)
    trampolineMonad.flatMap(nt) { i => 
      if (i <= 0) Done(true)
      else Bind(() => Done(i - 1), oddTrampoline)
    }
  }
  // run(evenTrampoline(9999)) does complete! YAY!

  object trampolines {

    sealed trait IO[F[_], +A]
    case class Pure[F[_], +A](get: A) extends IO[F,A]
    case class Request[F[_], I, +A](
      expr: F[I],
      receive: I => IO[F,A]) extends IO[F,A]
    case class BindMore[F[_],A,+B](
      force: () => IO[F,A],
      f: A => IO[F,B]) extends IO[F,B]
    case class BindRequest[F[_],I,A,+B](
      expr: F[I], receive: I => IO[F,A],
      f: A => IO[F,B]) extends IO[F,B]
    case class More[F[_],A](force: () => IO[F,A]) extends IO[F,A]

    trait Run[F[_]] {
      def apply[A](expr: F[A]): (A, Run[F])
    }

    // exercise 8
    @annotation.tailrec
    def run[F[_],A](R: Run[F])(io: IO[F,A]): A = io match {
      case Pure(get)           => get
      case Request(expr, recv) => R(expr) match { case (e, r2) => run(r2)(recv(e)) }
      case More(force)         => run(R)(force.apply)
      case BindMore(force, f)  => force.apply match {
        case Pure(get)                  => run(R)(f(get))
        case Request(expr, recv)        => run(R)(Request(expr, recv andThen f))
        case BindMore(force, g)         => run(R)(BindMore(force, f compose g))
        case BindRequest(expr, recv, g) => run(R)(BindRequest(expr, recv, f compose g))
        case More(force)                => run(R)(BindMore(force, f))
      }
      case BindRequest(expr, recv, f) => R(expr) match {
        case (i, r2) => run(r2)(BindMore(() => recv(i), f))
      }
    }
    def run[F[_],A](F: Monad[F])(io: IO[F,A]): F[A] =  io match {
      case Pure(get)           => F.unit(get)
      case More(force)         => run(F)(force.apply)
      case Request(expr, recv) => F.flatMap(expr)(i => run(F)(recv(i)))
      case BindRequest(expr, recv, f) => F.flatMap(expr) { i =>
        run(F)(BindMore(() => recv(i), f))
      }
      case BindMore(force, f) => force.apply match {
        case Pure(get)                  => run(F)(f(get))
        case Request(expr, recv)        => run(F)(Request(expr, recv andThen f))
        case BindMore(force, g)         => run(F)(BindMore(force, f compose g))
        case BindRequest(expr, recv, g) => run(F)(BindRequest(expr, recv, f compose g))
        case More(force)                => run(F)(BindMore(force, f))
      }
    }

    trait Future[+A] {
      def listen(cb: A => Trampoline[Unit]): Unit = ???
      def runAync(onFinish: A => Unit): Unit = ???
      def run: A = ???
    }
    object Future {
      case class Now[+A](a: A) extends Future[A]
      case class More[+A](thunk: () => Future[A]) extends Future[A]
      case class BindMore[A,B](
        thunk: () => Future[A],
        f: A => Future[B]) extends Future[B]

      case class Async[+A](onFinish: (A => Trampoline[Unit]) => Unit) extends Future[A]
      case class BindAsync[A,B](onFinish: (A => Trampoline[Unit]) => Unit,
                                f: A => Future[B]) extends Future[B]
    }

    // exercise 10
    val futureMonad = new Monad[Future] {
      def unit[A](a: => A): Future[A] = ???
      def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = ???
    }

    trait Trans[F[_], G[_]] {
      def apply[A](f: F[A]): G[A]
    }

    // exercise  11
    def run[F[_], G[_], A](T: Trans[F,G])(G: Monad[G])(io: IO[F,A]): G[A] = ???
  }
}
