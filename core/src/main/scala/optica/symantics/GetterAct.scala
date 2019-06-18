package optica
package symantics

import concrete.Getter

trait GetterAct[Repr[_], Obs[_]] {

  def get[S, A](gt: Repr[Getter[S, A]]): Obs[S => A]
}

object GetterAct {

  trait Syntax {

    implicit class GetterOps[Repr[_], Obs[_], S, A](
        gt: Repr[Getter[S, A]])(implicit
        ev: GetterAct[Repr, Obs]) {

      def get: Obs[S => A] = ev.get(gt)
    }
  }

  object syntax extends Syntax
}

