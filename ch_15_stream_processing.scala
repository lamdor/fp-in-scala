object ch15 {
  import ch13.IO

  def lines(filename: String): IO[Stream[String]] = IO {
    val src = io.Source.fromFile(filename)
    src.getLines.toStream append { src.close; Stream.empty }
  }

  case class Emit[I,O](
    head: Seq[O],
    tail: Process[I,O] = Halt[I,O]()
  ) extends Process[I,O] // means send out head, then change to tail
  case class Await[I,O](
    recv: I => Process[I,O],
    fallback: Process[I,O] = Halt[I,O]()
  ) extends Process[I,O] // means we need input, but do fallback if nothing is there
  case class Halt[I,O]() extends Process[I,O] // means stop

  trait Process[I,O] {
    import Process._

    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream.empty
      case Await(recv, fallback) => s match {
        case h #:: t => recv(h).apply(t)
        case _       => fallback.apply(s)
      }
      case Emit(h,t) => h.toStream append t.apply(s)
    }

    def map[O2](f: O => O2): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h,t) => Emit(h map f, t map f)
      case Await(recv, fb) => Await(recv andThen (_ map f), fb map f)
    }

    def ++(p: => Process[I,O]): Process[I,O] = this match {
      case Halt() => p
      case Emit(h,t) => emitAll(h, t ++ p)
      case Await(recv, fb) => Await(recv andThen (_ ++ p), fb ++ p)
    }

    def flatMap[O2](f: O => Process[I,O2]): Process[I,O2] = this match {
      case Halt() => Halt()
      case Emit(h,t) =>
        if (h.isEmpty) t flatMap f
        else f(h.head) ++ emitAll(h.tail, t).flatMap(f)
      case Await(recv, fb) =>
        Await(recv andThen (_ flatMap f), fb flatMap f)
    }

    // exercise 1
    def |>[O2](p2: Process[O,O2]): Process[I,O2] = this match {
      case Emit(h,t) => p2 match {
        case Emit(h2,t2) => Emit(h2, t |> t2.feed(h))
        case Await(recv2, fb2) => t |> p2.feed(h)
        case Halt() => Halt()
      }
      case Await(recv, fb) => p2 match {
        case Emit(h2, t2) => Await(i => recv(i) |> p2, fb |> p2)
        case Await(recv2, fb2) => Await(i => recv(i) |> p2, fb |> fb2)
        case Halt() => Halt()
      }
      case Halt() => Halt()
    }
    def feed(in: Seq[I]): Process[I,O] = this match {
      case Await(recv, fb) =>
        if (in.isEmpty) fb
        else recv(in.head).feed(in.tail)
      case Emit(h,t) =>
        Emit(h, t.feed(in))
      case Halt() => Halt()
    }

    def repeat: Process[I,O] = {
      def go(p: Process[I,O]): Process[I,O] = p match {
        case Halt() => go(this)
        case Await(recv, fb) => Await(recv andThen go, fb)
        case Emit(h,t) => Emit(h, go(t))
      }
      go(this)
    }

    // needed for exercise 6
    def zip[O2](p2: Process[I, O2]): Process[I, (O, O2)] = this match {
      case Halt() => Halt()
      case Emit(h,t) => p2 match {
        case Halt() => Halt()
        case Emit(h2,t2) => Emit(h.zip(h2), t.zip(t2))
        case Await(recv2, fb2) =>
          Await(i => this.zip(recv2(i)), this.zip(fb2))
      }
      case Await(recv, fb) => p2 match {
        case Halt() => Halt()
        case Await(recv2, fb2) =>
          Await(i => recv(i).zip(recv2(i)), fb.zip(fb2))
        case Emit(h2, t2) =>
          Await(i => recv(i).zip(p2), fb.zip(p2))
      }
    }

    // exercise 7
    def zipWithIndex: Process[I, (O,Int)] =
      this.zip(Process.count.map(_ - 1))
  }
  object Process {
    def emitAll[I,O](head: Seq[O], tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      tail match {
        case Emit(h2, tl) => Emit(head ++ h2, tl)
        case _            => Emit(head, tail)
      }

    def emit[I,O](head: O, tail: Process[I,O] = Halt[I,O]()): Process[I,O] =
      emitAll(Stream(head), tail)

    def lift[I,O](f: I => O): Process[I,O] =
      Await((i: I) => emit(f(i), lift(f)))

    def filter[I](f: I => Boolean): Process[I,I] =
      Await[I,I](i => if (f(i)) emit(i) else Halt()) repeat

    def sum: Process[Double, Double] = {
      def go(acc: Double): Process[Double, Double] =
        Await((d: Double) => emit(d + acc, go(d + acc)))
      go(0.0)
    }

    // exercise 2
    def take[I](n: Int): Process[I,I] = {
      def go(acc: List[I]): Process[I,I] =
        Await { i =>
          val soFar = acc :+ i
          if (soFar.size == n)
            emitAll(soFar)
          else
            go(acc :+ i)
        }
      go(Nil)
    }
    def id[I]: Process[I,I] = Await(i => emit(i, id))
    def drop[I](n: Int): Process[I,I] =
      Await { i =>
        if (n == 1)
          id
        else
          drop(n - 1)
      }
    def takeWhile[I](f: I => Boolean): Process[I,I] = {
      def go(acc: List[I]): Process[I,I] =
        Await { i =>
          if (f(i))
            go(acc :+ i)
          else
            emitAll(acc)
        }
      go(Nil)
    }
    def dropWhile[I](f: I => Boolean): Process[I,I] =
      Await { i =>
        if (f(i))
          dropWhile(f)
        else
          id
      }

    // exercise 3
    def count[I]: Process[I, Int] = {
      def go(n: Int): Process[I, Int] =
        Await(_ => emit(n, go(n + 1)))
      go(1)
    }

    // exercise 4
    def mean: Process[Double, Double] = {
      def go(n: Int, sum: Double): Process[Double, Double] =
        Await { d =>
          val sum_ = sum + d
          val n_ = n + 1
          emit(sum_ / n_, go(n_, sum_))
        }
      go(0,0)
    }

    def loop[S,I,O](z: S)(f: (I,S) => (O,S)): Process[I,O] =
      Await { (i: I) =>
        f(i,z) match {
          case (o,s2) => emit(o, loop(s2)(f))
        }
      }

    // exercise 5
    def sum_2: Process[Double, Double] =
      loop(0.0)((i,s) => (s + i, s + i))
    def count_2[I]: Process[I, Int] =
      loop(0)((_,s) => (s + 1, s + 1))

    // exercise 6
    def mean_2 = (sum_2 zip count_2) map { case (s, i) => s / i }


    // exercise 8
    def exists[I](f: I => Boolean): Process[I, Boolean] =
      Await { i =>
        if (f(i)) emit(true)
        else      exists(f)
      }
  }

  import ch12.Monad
  def monad[I]: Monad[({type l[x] = Process[I,x]})#l] =
    new Monad[({type l[x] = Process[I,x]})#l] {
      def unit[O](o: => O): Process[I,O] = Process.emit(o)
      def flatMap[O, O2](fa: Process[I,O])(f: O => Process[I,O2]): Process[I,O2] =
        fa flatMap f
    }

  import Process._
  def add1(i: Int): Int = {println(i); i + 1}
  val exampleProcess = emitAll(List(1,2,3,4)) |> (lift(add1) |> lift(add1))

  trait Source[O] {
    def |>[O2](p: Process[O,O2]): Source[O2]
    def filter(f: O => Boolean) = this |> Process.filter(f)
    def map[O2](f: O => O2) = this |> Process.lift(f)

    def collect: IO[IndexedSeq[O]]
  }

  case class ResourceR[R,I,O](
    acquire: IO[R],
    release: R => IO[Unit],
    step: R => IO[Option[I]],
    trans: Process[I,O]
  ) extends Source[O] {
    def |>[O2](p: Process[O,O2]) =
      ResourceR(acquire, release, step, trans |> p)

    def collect: IO[IndexedSeq[O]] = {
      def tryOr[A](a: => A)(cleanup: IO[Unit]) =
        try a finally cleanup.run

      @annotation.tailrec
      def go(acc: IndexedSeq[O],
             step: IO[Option[I]],
             p: Process[I,O],
             release: IO[Unit]): IndexedSeq[O] =
        p match {
          case Halt() => release.run; acc
          case Emit(h,t) =>
            go(acc ++ h, step, t, release)
          case Await(recv, fb) => tryOr(step.run)(release) match {
            case None => go(acc, IO(None), fb, release)
            case Some(i) => go(acc, step, tryOr(recv(i))(release), release)
          }
        }

      acquire map { res =>
        go(IndexedSeq(), step(res), trans, release(res))
      }
    }
  }
  object ResourceR {
    def lines(filename: String): Source[String] =
      ResourceR[io.Source, String, String](
        acquire = IO(io.Source.fromFile(filename)),
        release = (src) => IO(src.close),
        step    = (src) => {
          lazy val iter = src.getLines
          IO { if (iter.hasNext) Some(iter.next) else None }
        },
        Process.id[String]
      )
  }

  trait Sink[I] {
    def <|[I0](p: Process[I0,I]): Sink[I0]
    def filter(f: I => Boolean) = this <| Process.filter(f)
  }

  case class ResourceW[R,I,I2](
    acquire: IO[R],
    release: R => IO[Unit],
    recv: R => (I2 => IO[Unit]),
    trans: Process[I,I2]
  ) extends Sink[I] {
    def <|[I0](p: Process[I0,I]) =
      ResourceW(acquire, release, recv, p |> trans)
  }
  object ResourceW {
    import java.io.FileWriter
    def file(filename: String, append: Boolean = false): Sink[String] =
      ResourceW[FileWriter, String, String](
        acquire = IO(new FileWriter(filename, append)),
        release = (w) => IO(w.close),
        recv    = (w) => (s) => IO(w.write(s)),
        trans   = Process.id[String]
      )
  }
}
