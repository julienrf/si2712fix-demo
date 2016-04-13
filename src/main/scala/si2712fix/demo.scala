package si2712fix

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](f: A => B, fa: F[A]): F[B]
}

object Functor {
  implicit def function[A]: Functor[({ type l[B] = A => B })#l] =
    new Functor[({ type l[B] = A => B })#l] {
      def map[C, B](cb: C => B, ac: A => C): A => B = cb compose ac
    }
}

object FunctorSyntax {
  implicit class FunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(f, fa)
  }
}

trait CoFunctor[F[_]] {
  def comap[A, B](f: A => B, fb: F[B]): F[A]
}

object CoFunctor {
  implicit def function[B]: CoFunctor[({ type l[A] = A => B })#l] =
    new CoFunctor[({ type l[A] = A => B })#l] {
      def comap[A, C](ac: A => C, cb: C => B): A => B = ac andThen cb
    }
}

object CoFunctorSyntax {
  implicit class CoFunctorOps[F[_], A](fa: F[A])(implicit F: CoFunctor[F]) {
    def comap[B](f: B => A): F[B] = F.comap(f, fa)
  }
}

object Test {

  val f: Int => String = _.toString

  import FunctorSyntax._

  f.map((s: String) => s.reverse)

  import CoFunctorSyntax._

  f.comap((s: String) => s.length)

}
