object ch12 {
  import ch11._

  trait Applicative[F[_]] extends Functor[F] {
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
    def unit[A](a: => A): F[A]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      map2(fa, unit(()))((a,_) => f(a))

    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List.empty[B]))((a,fbs) => map2(f(a), fbs)(_ :: _))

    // exercise 1
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)
    // fas.foldRight(unit(List.empty[A]))((fa, fas) => map2(fa, fas)(_ :: _))

    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      traverse((1 to n).toList)(_ => fa)

    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
      map2(fa, fb)(Pair.apply)

    // exercise 2
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)((f,a) => f(a))

    def map3[A,B,C,D](fa: F[A],
                      fb: F[B],
                      fc: F[C])(f: (A,B,C) => D): F[D] =
      apply(
        apply(
          apply(
            unit(f.curried)
          )(fa)
        )(fb)
      )(fc)

    // exercise 11
    def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
      ofa.foldRight(unit(Map.empty[K,V])) { (kfv, fm) =>
        val (k,fv) = kfv
        map2(fm, fv)((m,v) => m + (k -> v))
      }
  }

  // exercise 2
  trait ApplicativeByApply[F[_]] extends Functor[F] {
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
    def unit[A](a: => A): F[A]

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
      apply(map(fb)(b => ((a: A) => f(a,b))))(fa)

    // exercise 3
    def map3[A,B,C,D](fa: F[A],
                      fb: F[B],
                      fc: F[C])(f: (A,B,C) => D): F[D] =
      apply(
        apply(
          apply(
            unit(f.curried)
          )(fa)
        )(fb)
      )(fc)
  }

  trait Monad[F[_]] extends Applicative[F] {
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

    def map2[A,B,C](fa: F[A],fb: F[B])(f: (A,B) => C): F[C] =
      flatMap(fa)(a => map(fb)(b => f(a,b)))
    // def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    //   flatMap(fab)(f => map(fa)(f))

    def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
    def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
      a => flatMap(f(a))(g)
    // override def map[A, B](fa: F[A])(f: A => B): F[B] =
    //   flatMap(fa)(a => unit(f(a)))
    // override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    //   flatMap(fa)(a => map(fb)(b => f(a,b)))
  }

  // exercise 5
  def eitherMonad[E] = new Monad[({type l[X] = Either[E,X]})#l]  {
    def flatMap[A,B](fa: Either[E,A])(f: A => Either[E,B]): Either[E, B] = fa match {
      case Right(a) => f(a)
      case Left(e)  => Left(e)
    }
    def unit[A](a: => A): Either[E,A] = Right(a)
  }

  sealed trait Validation[+E, +A]
  case class Failure[E](head: E, tail: Vector[E] = Vector.empty) extends Validation[E,Nothing]
  case class Success[A](value: A) extends Validation[Nothing,A]

  // exercise 6
  def validationApplicative[E] = new Applicative[({type l[A] = Validation[E,A]})#l] {
    def map2[A,B,C](v1: Validation[E, A], v2: Validation[E, B])(f: (A,B) => C): Validation[E, C] = (v1, v2) match {
      case (Failure(head1, tail1), Failure(head2, tail2)) =>
        Failure(head1, tail1 ++ (head2 +: tail2))
      case (f: Failure[_], s: Success[_]) => f
      case (s: Success[_], f: Failure[_]) => f
      case (Success(a), Success(b)) => unit(f(a,b))
    }
    def unit[A](a: => A): Validation[E, A] = Success(a)
  }

  import java.util.Date
  case class WebForm(name: String, birthDate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name.nonEmpty) Success(name) else Failure("empty name")

  def validBirthDate(birthDate: String): Validation[String, Date] =
    try {
      import java.text.SimpleDateFormat
      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthDate))
    } catch {
      case e: Throwable => Failure("Birth date must be in the form yyyy-MM-dd")
    }

  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}")) Success(phoneNumber) else Failure("phone number must be 10 digits")

  def validWebForm(name: String, birthDate: String, phoneNumber: String): Validation[String, WebForm] =
    validationApplicative[String].map3(validName(name),
                                       validBirthDate(birthDate),
                                       validPhone(phoneNumber))(WebForm(_,_,_))

  def assoc[A,B,C](p: (A,(B,C))): ((A,B),C) = p match {
    case (a,(b,c)) => ((a,b),c)
  }

  def productF[I,O,I2,O2](f: I => O, g: I2 => O2): (I,I2) => (O,O2) =
    (i, i2) => (f(i), g(i2))

  import ch8._
  import Gen._
  import SGen._
  import Prop._
  def applicativeLaws[A, F[A]](A: Applicative[F], genF: Gen[F[A]], genaf: Gen[A => A]) = {
    import A._

    forAll(genF) { fa =>  // identity
      map2(unit(()), fa)((_, a) => a) == fa &&
      map2(fa, unit(()))((a, _) => a) == fa
    } &&
    forAll(genF ** genF ** genF) { case ((fa,fb),fc) => // associativity
      product(product(fa,fb), fc) == map(product(fa, product(fb,fc)))(assoc)
    } &&
    forAll(genF ** genF ** genaf ** genaf) { case (((a, b), f), g) =>
      map2(a, b)(productF(f,g)) == product(map(a)(f), map(b)(g))
    }
  }

  // exercise 8
  def productApplicative[F[_], G[_]](F: Applicative[F], G: Applicative[G]):
      Applicative[({type l[x] = (F[x],G[x])})#l] =
    new Applicative[({type l[x] = (F[x],G[x])})#l] {
      def map2[A,B,C](fa: (F[A],G[A]), fb: (F[B], G[B]))(f: (A,B) => C): (F[C], G[C]) =
        (
          F.map2(fa._1, fb._1)(f),
          G.map2(fa._2, fb._2)(f)
        )
      def unit[A](a: => A): (F[A], G[A]) =
        (F.unit(a), G.unit(a))
    }

  // exercise 9
  def compose[F[_], G[_]](F: Applicative[F], G: Applicative[G]):
      Applicative[({type l[x] = F[G[x]]})#l] =
    new Applicative[({type l[x] = F[G[x]]})#l] {
      def map2[A,B,C](fa: F[G[A]], fb: F[G[B]])(f: (A,B) => C): F[G[C]] =
        F.map2(fa, fb)((ga,gb) => G.map2(ga,gb)(f))
      def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    }

  trait Traverse[F[_]] extends Functor[F]  with Foldable[F]{
    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))
    def sequence[G[_]: Applicative, A, B](fa: F[G[A]]): G[F[A]] =
      traverse(fa)(identity)

    // exercise 13
    type Id[A] = A
    val idApplicative = new Applicative[Id] {
      def map2[A,B,C](ia: Id[A], ib: Id[B])(f: (A,B) => C): Id[C] = f(ia,ib)
      def unit[A](a: => A): Id[A] = a
    }
    def map[A,B](fa: F[A])(f: A => B): F[B] =
      // if you squint, sort of looks like traverse(fa: F[A])(f: A => Id[B]): Id[F[B]]
      traverse[Id,A,B](fa)(f)(idApplicative)

    // from below's Foldable and Const types
    import ch10.Monoid
    def foldMap[A,M](as: F[A])(f: A => M)(M: Monoid[M]): M =
      traverse[({type l[x] = Const[M,x]})#l,A,Nothing](as)(f)(monoidApplicative(M))

    import ch6.State
    def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] =
      traverse[({type l[x] = State[S,x]})#l,A,B](fa)(f)(stateMonad[S])
    import State._
    def zipWithIndex[A](fa: F[A]): F[(A,Int)] =
      traverseS(fa)(a => (for {
                            i <- get[Int]
                            _ <- set(i + 1)
                          } yield (a,i))).run(0)._1

    override def toList[A](fa: F[A]): List[A] =
      traverseS(fa)(a => (for {
                            as <- get[List[A]]
                            _  <- set(a :: as)
                          } yield ())).run(Nil)._2.reverse

    def mapAccum[S,A,B](fa: F[A], s: S)(f: (A,S) => (B,S)): (F[B],S) =
      traverseS(fa)(a => (for {
                            s1 <- get[S]
                            (b, s2) = f(a,s1)
                            _ <- set(s2)
                          } yield b)).run(s)

    def zipWithIndex_2[A](fa: F[A]): F[(A,Int)] =
      mapAccum(fa, 0)((a,i) => ((a,i), i + 1))._1
    def toList_2[A](fa: F[A]): List[A] =
      mapAccum(fa, List.empty[A])((a, as) => ((), a :: as))._2.reverse

    // exercise 15 (cheaded)
    def reverse[A](fa: F[A]): F[A] =
      mapAccum(fa, toList(fa))((_, as) => (as.head, as.tail))._1

    def zip[A,B](fa: F[A],fb: F[B]): F[(A,B)] =
      (mapAccum(fa, toList(fb)) {
         case (a, Nil) => sys.error("zip: wrong shapes")
         case (a, b :: bs) => ((a,b), bs)
       })._1
    def zipL[A,B](fa: F[A],fb: F[B]): F[(A,Option[B])] =
      (mapAccum(fa, toList(fb)) {
         case (a, Nil) => ((a,None), Nil)
         case (a, b :: bs) => ((a,Some(b)), bs)
       })._1

    def zipR[A,B](fa: F[A],fb: F[B]): F[(Option[A],B)] =
      (mapAccum(fb, toList(fa)) {
         case (b, Nil) => ((None, b), Nil)
         case (b, a :: as) => ((Some(a),b), as)
       })._1

    // exercise 17 (cheated)
    def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], h: A => H[B])
                           (G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
      traverse[({type l[x] = (G[x], H[x])})#l,A,B](fa)(a => (f(a), h(a)))(productApplicative(G,H))

    // exercise 18 :(
    // def compose[G[_]](implicit G: Traverse[G]): Traverse[({type l[x] = F[G[x]]})#l] =
    //   new Traverse[({type l[x] = F[G[x]]})#l] {
    //     override def traverse[H[_]: Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] = traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
    //   }
  }

  // exercise 12
  val listTraverse = new Traverse[List] {
    override def traverse[G[_]: Applicative, A, B](as: List[A])(f: A => G[B]): G[List[B]] = {
      val G = implicitly[Applicative[G]]
      as.foldRight(G.unit(List.empty[B]))((a,gbs) => G.map2(f(a), gbs)(_ :: _))
    }
  }
  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_]: Applicative, A, B](oa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val G = implicitly[Applicative[G]]
      val ofb: Option[G[B]] = map(oa)(f)
      val ofob: Option[G[Option[B]]] = map(ofb)(fb => G.map(fb)(Option.apply))
      ofob getOrElse G.unit(None)
    }
  }
  case class Tree[+A](head: A, tail: List[Tree[A]] = Nil)
  val treeTraverse  = new Traverse[Tree] {
    override def traverse[G[_]: Applicative, A, B](fa: Tree[A])(f: A => G[B]): G[Tree[B]] = {
      val G = implicitly[Applicative[G]]
      val Tree(head, tail) = fa

      // basically taking that G and pushing it up
      // listTraverse.traverse[G,A,B](tail: List[Tree[A]])(
      //   (tree => traverse[G,A,B](tree)(f)): Tree[A] => G[Tree[B]]
      // )
      def transformTail(tail: List[Tree[A]]): G[List[Tree[B]]] =
        listTraverse.traverse(tail)(traverse(_)(f))
      
      G.map2(f(head), transformTail(tail))(Tree.apply)
    }
  }

  implicit val optionApplicative = new Applicative[Option] {
    def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A,B) => C): Option[C] =
      oa.flatMap(a => ob.map(b => f(a,b)))
    def unit[A](a: => A): Option[A] = Some(a)
  }

  val exampleTree: Tree[Option[Int]] = Tree(Some(1),
                                            List(Tree(Some(2)),
                                                 Tree(Some(3),
                                                      List(Tree(Some(4)), // None
                                                           Tree(Some(5))))))

  import ch10.Monoid
  type Const[M,B] = M
  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type l[x] = Const[M,x]})#l] {
      def unit[A](a: => A) = M.zero
      def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1,m2)
    }

  // foldable (modified a little; "combinates" from foldMap) from ch 10
  trait Foldable[F[_]] {
    def foldMap[A,M](as: F[A])(f: A => M)(mb: Monoid[M]): M

    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B = {
      val foldLeftMonoid = new Monoid[(B,Option[A])] {
        def zero = (z, None)
        def op(ba1: (B, Option[A]), ba2: (B, Option[A])) = {
          val (b1, a1) = ba1
          val (b2, a2) = ba2
          (a1, a2) match {
            case (Some(a1), Some(a2)) => (f(b1,a1), Some(a2))
            case _ => zero
          }
        }
      }
      foldMap(as)(a => (z, Option(a)))(foldLeftMonoid)._1
    }
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B = foldLeft(as)(z)((b,a) => f(a,b)) // just lazy
    def toList[A](fa: F[A]): List[A] = foldRight(fa)(List.empty[A])(_ :: _)
  }

  import ch6.State
  // needed because our previous monad in ch11 didn't extend applicative
  def stateMonad[S] = new Monad[({type l[x] = State[S,x]})#l] {
    def unit[A](a: => A): State[S,A] = State.unit(a)
    def flatMap[A, B](fa: State[S,A])(f: A => State[S,B]): State[S,B] =
      fa flatMap f
  }

  // exercise 19
  def composeM[F[_], G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]):
      Monad[({type l[x] = F[G[x]]})#l] = new Monad[({type l[x] = F[G[x]]})#l] {
    def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    def flatMap[A, B](fa: F[G[A]])(f: A => F[G[B]]): F[G[B]] =
      F.flatMap(fa) { (ga: G[A]) =>
        F.map(T.sequence(G.map(ga)(f))(F))(G.join)
      }
  }

  case class OptionT[M[_], A](value: M[Option[A]])(implicit M: Monad[M]) {
    def flatMap[B](f: A => OptionT[M,B]): OptionT[M,B] =
      OptionT(M.flatMap(value) { 
                case None => M.unit(None)
                case Some(a) => f(a).value
              })
  }
}
