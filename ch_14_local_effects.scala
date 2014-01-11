object ch14 {
  sealed trait ST[S,A] { self =>
    protected def run(s: S): (A,S)
    def map[B](f: A => B): ST[S,B] = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }
    def flatMap[B](f: A => ST[S,B]) = new ST[S,B] {
      def run(s: S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S,A](a: => A) = {
      lazy val memo = a
      new ST[S,A] {
        def run(s: S) = (a,s)
      }
    }

    def empty[S] = ST[S,Unit](())

    def runST[A](st: RunnableST[A]): A =
      st.apply[Null].run(null)._1
  }

  import ch12._
  def stMonad[S] = new Monad[({type l[x] = ST[S,x]})#l] {
    def unit[A](a: => A): ST[S,A] = ST(a)
    def flatMap[A, B](fa: ST[S,A])(f: A => ST[S,B]): ST[S,B] = fa flatMap f
  }

  sealed trait STRef[S,A] { self =>
    protected var cell: A
    def read: ST[S,A] = ST(cell)
    def write(a: A): ST[S,Unit] = ST { self.cell = a }
    def modify[B](f: A => A): ST[S,Unit]  = ST { self.cell = f(self.cell) }
  }
  object STRef {
    def apply[S,A](a: A): ST[S, STRef[S,A]] = ST(new STRef[S,A] {
                                                   var cell = a
                                                 })
  }

  trait RunnableST[A] {
    def apply[S]: ST[S,A]
  }

  val swapNumbers = new RunnableST[(Int,Int)] {
    def apply[S] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y + 1)
      _ <- r2.write(x + 1)
      a <- r1.read
      b <- r2.read
    } yield (a,b)
  }

  sealed abstract class STArray[S,A] {
    protected def value: Array[A]
    def write(i: Int, a: A): ST[S,Unit] = new ST[S,Unit] {
      def run(s: S) = {
        value(i) = a
        ((), s)
      }
    }

    def read(i: Int): ST[S,A] = ST(value(i))
    def size: ST[S, Int] = ST(value.size)
    def freeze: ST[S,List[A]] = ST(value.toList)

    // exercise 1
    def fill(xs: Map[Int,A]): ST[S, Unit] =
      ST(xs.foreach { case (k,v) => value(k) = v})

    def swap(i: Int, j: Int): ST[S,Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i,y)
      _ <- write(j,x)
    } yield ()
  }

  object STArray {
    def apply[S,A: Manifest](sz: Int, v: A): ST[S, STArray[S,A]] =
      ST {
        new STArray[S,A] {
          lazy val value = Array.fill(sz)(v)
        }
      }
    def fromList[S,A:Manifest](xs: List[A]): ST[S,STArray[S,A]] =
      ST {
        new STArray[S, A] {
          lazy val value = xs.toArray
        }
      }
  }

  // exercise 2
  def partition[S](arr: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = {
    def swapIfLessAndMoveAlong(jrst: ST[S, STRef[S,Int]], pv: Int)(i: Int): ST[S,Unit] = {
      for {
        jr <- jrst; j <- jr.read
        a <- arr.read(i)
        _ <- (if (a < pv) {
                for { _ <- arr.swap(i, l + j)
                      _ <- jr.modify(_ + 1) } yield ()
              } else ST.empty[S])
      } yield ()
    }

    val jrst = STRef[S, Int](0)

    for {
      pv <- arr.read(pivot)
      _ <- arr.swap(pivot,r)
      jr <- jrst
      _ <-
        listTraverse.traverse[({type l[x] = ST[S,x]})#l, Int, Unit](
          (l until r).toList
        )(swapIfLessAndMoveAlong(jrst, pv))(stMonad[S])
      j <- jr.read
      _ <- arr.swap(j,r)
    } yield j
  }

  def qs[S](a: STArray[S,Int], l: Int, r: Int): ST[S,Unit] =  {
    if (l < r) 
      for {
        pi <- partition(a, l, r, l + (r - l) / 2)
        _ <- qs(a, l, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    else
      ST.empty
  }

  def quicksort(xs: List[Int]): List[Int] =
    if (xs.isEmpty) xs
    else ST.runST(new RunnableST[List[Int]] {
                    def apply[S] = for {
                      arr <- STArray.fromList(xs)
                      size <- arr.size
                      _ <- qs(arr, 0, size - 1)
                      sorted <- arr.freeze
                    } yield sorted
                  })

}
