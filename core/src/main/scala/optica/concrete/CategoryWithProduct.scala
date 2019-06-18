package optica
package concrete

import scalaz._

trait CategoryWithProduct[=>:[_, _], **[_, _]] extends Category[=>:] {

  def pi1[A, B]: (A ** B) =>: A

  def pi2[A, B]: (A ** B) =>: B

  def fork[S, A, B](f: S =>: A, g: S =>: B): S =>: (A, B)
}

object CategoryWithProduct {

  trait CategoryWithProductSyntax {

    implicit class CategoryWithProductOps[=>:[_, _], **[_, _], S, A](
        f: S =>: A)(implicit
        ev: CategoryWithProduct[=>:, **]) {
      def ***[B](g: S =>: B): S =>: (A, B) = ev.fork(f, g)
    }
  }

  object syntax extends CategoryWithProductSyntax
}

