object ch5 {
  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def maybeTwice(b: Boolean, i: => Int) = if (b) i + i else 0
  def maybeTwice2(b: Boolean, i: => Int) = {
    lazy val j = i
    if (b) j+j else 0
  }

  // for console puposes
  def printAndGive(i: Int): Int = {
    println(i)
    i
  }

  sealed abstract class Stream[+A] {
    def uncons: Option[Cons[A]]
    def isEmpty: Boolean = uncons.isEmpty

    // exercise 1
    def toList: List[A] = {
      @annotation.tailrec
      def go(as: Stream[A], soFar: List[A]): List[A] = as.uncons match {
        case None => soFar
        case Some(cons) =>
          go(cons.tail, soFar :+ cons.head)
      }
      go(this, Nil)
    }

    // exercise 2
    def take(n: Int): Stream[A] = 
      if (n > 0) {
        uncons match {
          case None       => Empty
          case Some(cons) => Stream.cons(cons.head, cons.tail.take(n - 1))
        }
      }
      else {
        Stream.empty
      }

    // exercise 3
    def takeWhile(p: A => Boolean): Stream[A] =
      uncons match {
        case None       => Empty
        case Some(cons) => new Stream[A] {
          lazy val uncons =
            if (p(cons.head)) {
              val updatedCons = Stream.cons_(cons.head,
                                             cons.tail.takeWhile(p))
              Some(updatedCons)
            } else {
              None
            }
        }
      }

    def exists(p: A => Boolean): Boolean =
      uncons match {
        case Some(c) => p(c.head) || c.tail.exists(p)
        case None    => false
      }

    def foldRight[B](z: => B)(f: (=> A, => B) => B): B =
      uncons match {
        case Some(c) => f(c.head, c.tail.foldRight(z)(f))
        case None    => z
      }

    def exists_2(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    // exercise 4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a,b) => p(a) && b)

    // exercise 5
    def takeWhile_2(p: A => Boolean): Stream[A] = {
      foldRight(Stream.empty[A]) { (a,s) =>
        new Stream[A] {
          lazy val uncons =
            if (p(a))
              Some(Stream.cons_(a,s))
            else
              None
        }
      }
    }

    // exercise 6
    def uncons_2: Option[Cons[A]] =
      foldRight(None: Option[Cons[A]]) {(a, b) =>
        val tail = b match {
          case Some(t) => t
          case None    => Stream.empty
        }
        Some(Stream.cons_(a, tail))
      }
    def foldRight_2[B](z: => B)(f: (=> A, => B) => B): B = ???

    // exercise 7
    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((a,s) => Stream.cons(f(a), s))
    def filter(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A]) { (a,s) =>
        new Stream[A] {
          lazy val uncons =
            if(p(a))
              Some(Stream.cons_(a, s.filter(p)))
            else
              s.filter(p).uncons
        }
      }
    def append[B >: A](b: => B) =
      foldRight(Stream(b))(Stream.cons)
    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B]) { (a,s) =>
        new Stream[B] {
          lazy val uncons =
            f(a).foldRight(s)(Stream.cons).uncons
        }
      }

    def find(p: A => Boolean): Option[A] =
      filter(p).uncons.map(_.head)

    // exercise 13
    def map_2[B](f: A => B): Stream[B] =
      Stream.unfold(this) { (s) =>
        s.uncons.map(c => (f(c.head), c.tail))
      }
    def take_2(n: Int): Stream[A] =
      Stream.unfold((this, n)) { case (s,i) =>
        if (i > 0) {
          s.uncons.map(c => (c.head, (c.tail, i - 1)))
        } else {
          None
        }
      }
    def takeWhile_3(p: A => Boolean): Stream[A] =
      Stream.unfold(this) { (s) =>
        s.uncons.flatMap { c =>
          if (p(c.head))
            Some(c.head, c.tail)
          else
            None
        }
      }
    def zip[B](bs: Stream[B]): Stream[(A,B)] =
      Stream.unfold((this, bs)) { case (as, bs) =>
        (as.uncons, bs.uncons) match {
          case (Some(aCons), Some(bCons)) =>
            Some((aCons.head, bCons.head),
                 (aCons.tail, bCons.tail))
          case _ =>
            None
        }
      }
    def zipAll[B](bs: Stream[B]): Stream[(Option[A],Option[B])] =
      Stream.unfold((this, bs)) { case (as, bs) =>
        (as.uncons, bs.uncons) match {
          case (None, None) => None
          case (ac, bc) =>
            Some(
              ((ac.map(_.head),
                bc.map(_.head)),
               (ac.map(_.tail).getOrElse(Stream.empty),
                bc.map(_.tail).getOrElse(Stream.empty)))
            )
        }
      }

    // exercise 14
    def startsWith[B >: A](bs: Stream[B]): Boolean =
      zipAll(bs) map {
        case (Some(a), Some(b)) => Some(a == b)
        case (None,    Some(_)) => Some(false)
        case (_,       None)    => None
      } takeWhile (_.isDefined) forAll (_.exists(identity))

    // exercise 15
    def tails: Stream[Stream[A]] =
      Stream.unfold(this) { s =>
        s.uncons.map { cons =>
          (s, cons.tail)
        }
      }

    def hasSubsequence[B >: A](bs: Stream[B]): Boolean =
      tails exists (_.startsWith(bs))

    // exercise 16
    def scanRight[B](z: B)(f: (A,B) => B): Stream[B] = {
      val orig = this
      new Stream[B] {
        def uncons = orig.uncons match {
          case Some(cons) =>
            val rest = cons.tail.scanRight(z)(f)
            val headrest = rest.uncons.map(_.head).getOrElse(z)
            Some(Stream.cons_(f(cons.head, headrest), rest))
          case None => None
        }
      }
    }
    def tails_2: Stream[Stream[A]] =
      scanRight(Stream.empty[A])((a, as) => Stream.cons(a,as))
  }
  object Empty extends Stream[Nothing] {
    val uncons = None

    // exercise 6
    override def foldRight_2[B](z: => B)(f: (=> Nothing, => B) => B): B = z
  }
  sealed abstract class Cons[+A] extends Stream[A] {
    def head: A
    def tail: Stream[A]
    val uncons = Some(this)

    // exercise 6
    override def foldRight_2[B](z: => B)(f: (=> A, => B) => B): B =
      f(head, tail.foldRight_2(z)(f))
  }
  object Stream {
    def empty[A]: Stream[A] = Empty

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      cons_(hd, tl)

    def cons_[A](hd: => A, tl: => Stream[A]): Cons[A] = new Cons[A] {
      lazy val head = hd
      lazy val tail = tl
    }

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def test: Stream[Int] =
      Stream.cons(printAndGive(1),
                  Stream.cons(printAndGive(2),
                              Stream.cons(printAndGive(3),
                                          Stream.empty)))

    val ones: Stream[Int] = cons(1, ones)

    // exercise 8
    def constant[A](a: A): Stream[A] =
      cons(a, constant(a))
    val ones_2 = constant(1)

    // exercise 9
    def from(n: Int): Stream[Int] =
      cons(n, from(n + 1))

    // exercise 10
    def fibs(n: Int = 0, m: Int = 1): Stream[Int] =
      cons(n, fibs(m, n + m))

    // exercise 11
    def unfold[A, S](z: => S)(f: S => Option[(A,S)]): Stream[A] =
      new Stream[A] {
        lazy val uncons =
          f(z).map {
            case (a,s) =>
              Stream.cons_(a, unfold(s)(f))
          }
      }

    // exercise 12
    def fibs_2(n: Int = 0, m: Int = 1) =
      unfold((n,m)) { case (n,m) => Some(n, (m, n + m)) }
    def from_2(n: Int) =
      unfold(n)(i => Some(i, i + 1))
    def constant_2[A](a: A) =
      unfold(a)(a => Some(a,a))
    val ones_3 = unfold(1)(_ => Some(1,1))
  }

}
