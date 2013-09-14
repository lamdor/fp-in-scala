object ch3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  // exercise 1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // <- 3
    case Cons(h,t) => h + List.sum(t)
    case _ => 101
  }

  object List {
    def empty[A]: List[A] = Nil

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // exercise 2
    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }

    // exercise 3
    def setHead[A](a: A, as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(_, tail) => Cons(a, tail)
    }

    // exercise 4
    @annotation.tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Cons(_, as) if n > 0 => drop(as, n - 1)
      case _                    => l
    }

    // exericse 5
    @annotation.tailrec
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Cons(a, as) if f(a) => dropWhile(as)(f)
      case _                   => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    // exercise 6
    def init[A](l: List[A]): List[A] = l match {
      case Nil          => Nil
      case Cons(a, Nil) => Nil
      case Cons(a, as)  => Cons(a, init(as))
    }

    def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(l: List[Int]): Int =
      foldRight(l, 0)(_ + _)

    def product2(l: List[Double]): Double =
      foldRight(l, 1.0)(_ * _)

    // exercise 9
    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, l) => l + 1)

    // exercise 10
    def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
      @annotation.tailrec
      def go(l: List[A], b: B, f: (B,A) => B): B = l match {
        case Nil => b
        case Cons(a, as) => go(as, f(b, a), f)
      }

      go(l, z, f)
    }

    // exercise 11
    def sum3(l: List[Int]): Int =
      foldLeft(l, 0)(_ + _)

    def product3(l: List[Double]): Double =
      foldLeft(l, 1.0)(_ * _)

    def length3[A](l: List[A]): Int =
      foldLeft(l, 0)((b, _) => b + 1)

    // exercise 12
    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, List.empty[A])((as, a) => Cons(a, as))

    // exercise 13
    def foldLeft2[A,B](l: List[A], z: B)(f: (B,A) => B) =
      foldRight(l, z)((a,b) => f(b,a)) // not quite right

    // exercise 14
    def append2[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1, a2)(Cons.apply)

    // exercise 15
    def appendAll[A](ass: List[List[A]]): List[A] =
      foldRight(ass, List.empty[A])(append2)

    // exercise 16
    def add1ToEach(l: List[Int]): List[Int] = l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(x + 1, add1ToEach(xs))
    }

    // exercise 17
    def turnEachDoubleToString(l: List[Double]): List[String] = l match {
      case Nil => Nil
      case Cons(x,xs) => Cons(x.toString, turnEachDoubleToString(xs))
    }

    // exercise 18
    def map[A,B](l: List[A])(f: A => B): List[B] = l match {
      case Nil => Nil
      case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }
    def map2[A,B](l: List[A])(f: A => B): List[B] =
      foldRight(l, List.empty[B])((a,b) => Cons(f(a), b))

    // exercise 19
    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      foldRight(l, List.empty[A]) { (a, as) =>
        if (f(a)) {
          Cons(a, as)
        } else {
          as
        }
      }

    // exercise 20
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
      foldRight(l, List.empty[B]) { (a, bs) =>
        foldRight(f(a), bs)(Cons.apply)
      }
    def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] =
      foldRight(l, List.empty[B]) { (a, bs) =>
        append(f(a), bs)
      }

    // exercise 21
    def filter2[A](l: List[A])(f: A => Boolean): List[A] =
      flatMap(l) { a => 
        if (f(a))
          List(a)
        else
          Nil
      }

    // exercise 22
    def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
      case Nil => Nil
      case Cons(i1, is1) => l2 match {
        case Nil => Nil
        case Cons(i2, is2) =>
          Cons(i1 + i2, zipAdd(is1, is2))
      }
    }

    // exercise 23
    def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = as match {
      case Nil => Nil
      case Cons(a, as) => bs match {
        case Nil => Nil
        case Cons(b, bs) =>
          Cons(f(a,b), zipWith(as,bs)(f))
      }
    }
    def zipAdd2(l1: List[Int], l2: List[Int]): List[Int] =
      zipWith(l1, l2)(_ + _)

    // exercise 24
    def take[A](l: List[A], n: Int): List[A] = l match {
      case Nil         => Nil
      case _ if n <= 0 => Nil
      case Cons(a, as) => Cons(a, take(as, n - 1))
    }

    def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
      case Nil => false
      case l@Cons(_, tail) =>
        val sizes = (1 to length(l))
        sizes.exists(take(l, _) == sub) ||
          hasSubsequence(tail, sub)
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // exercise 25
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

    // exercise 26
    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(i) => i
      case Branch(left, right) => math.max(maximum(left),
                                           maximum(right))
    }

    // exercise 27
    def depth(t: Tree[_]): Int = t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + math.max(depth(left),
                                               depth(right))
    }

    // exercise 28
    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) =>
        Branch(map(left)(f), map(right)(f))
    }

    // exercise 29
    // not sure i have this fold right (bad pun)
    def fold[A,B](t: Tree[A], zero: B)(f: (A, B) => B): B = t match {
      case Leaf(a) => f(a, zero)
      case Branch(left, right) =>
        fold(right, fold(left, zero)(f))(f)
    }
    def size2(t: Tree[_]): Int = fold(t, 0)((_, b) => b + 1)
    def maximum2(t: Tree[Int]): Int =
      fold(t, Int.MinValue)(_ max _)
  }
}
