import java.util.concurrent._

object ch7 {
  type Par[A] = ExecutorService => Future[A]

  object Par {
    // exercise 3
    def unit[A](a: A): Par[A] = {
      es =>
      new Future[A] {
        def cancel(mayInteruppt: Boolean) = false
        def get() = a
        def get(timeout: Long, unit: TimeUnit) = a
        def isCancelled() = false
        def isDone() = true
      }
    }

    def async[A](a: => A): Par[A] = fork(unit(a))

    def fork[A](a: => Par[A]): Par[A] = { es =>
      val c = callable(a(es).get)
      es.submit(c)
    }
    def delay[A](fa: => Par[A]): Par[A] = { es =>
      fa(es)
    }

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    // exercise 4
    def asyncF[A,B](f: A => B): A => Par[B] = { a =>
      async(f(a))
    }

    // exercise 1
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = { es =>
      val futA = a(es)
      val futB = b(es)
      async {
        f(futA.get, futB.get)
      } apply es
    }

    def map[A,B](fa: Par[A])(f: A => B): Par[B] =
      map2(fa, unit(()))((a,_) => f(a))

    private[this] def callable[A](a: => A): Callable[A] = new Callable[A] {
      def call() = a
    }

    // exercise 2
    // my representation of Par[A]
    // trait Par[A] { def get: A }
    // case class UnitPar[A](get: A) extends Par
    // case class ForkedPar[A](a: => A)(implicit ec: ExecutionContext) extends Par {
    //   def get = ec.sumbit(a).get
    // }

    // exercise 5
    def sequence[A](l: List[Par[A]]): Par[List[A]] = { es =>
      val fs = l.map(_ apply es)
      async(fs.map(_.get)) apply es
    }

    def parMap[A,B](l: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = l.map(asyncF(f))
      sequence(fbs)
    }

    // exercise 6
    def parFilter[A](l: List[A])(p: A => Boolean): Par[List[A]] = fork {
      val fs = l.map(asyncF(a => (p(a), a)))
      map(sequence(fs)) { l =>
        l.collect {
          case (true, a) => a
        }
      }
    }

    def equal[A](es: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean =
      (map2(p1, p2)(_ == _) apply es).get

    // exercise 11
    def choice[A](test: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] = { es =>
      if (test(es).get)
        ifTrue(es)
      else
        ifFalse(es)
    }

    // exercise 12
    def choiceN[A](choose: Par[Int])(choices: List[Par[A]]): Par[A] = { es =>
      val index = choose(es).get
      choices(index)(es)
    }
    def choice_2[A](test: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(Par.map(test)(if (_) 0 else 1))(List(ifTrue, ifFalse))

    // exercise 13
    def choiceMap[A,B](choose: Par[A])(choices: Map[A, Par[B]]): Par[B] = { es =>
      val key = choose(es).get
      choices(key)(es)
    }

    // exercise 14
    def flatMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] = { es =>
      val a = pa(es).get
      f(a)(es)
    }
    def choiceN_2[A](choose: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(choose)(i => choices(i))
    def choiceMap_2[A,B](choose: Par[A])(choices: Map[A, Par[B]]): Par[B] =
      flatMap(choose)(a => choices(a))

    // exercise 15
    def join[A](a: Par[Par[A]]): Par[A] = flatMap(a)(identity)
  }

  import Par._
  def sum(as: IndexedSeq[Int]): Par[Int] =
    if (as.size <= 1) unit(as.headOption getOrElse 0)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(fork(sum(l)), fork(sum(r)))(_ + _)
    }

  val to100 = (1 to 100).toIndexedSeq
  lazy val es: ExecutorService = Executors.newFixedThreadPool(20)

  def sortPar(l: Par[List[Int]]): Par[List[Int]] =
    map(l)(_.sorted)

  // exercise 10
  // type Par2[A,B,C] = ExecutorService => (A => Par2[_, B, C])

}
