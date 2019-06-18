package example
package couple

import optica._
import concrete._
import symantics._

import Optica.syntax._

object Logic {

  def differencesFl[Repr[_]](implicit
      O: Optica_[Repr],
      M: Model[Repr]): Repr[Fold[Couples, (String, Int)]] = {
    import O._, M._
    couples >>> filtered((her >>> age) > (him >>> age)) >>>
                (her >>> name) *** ((her >>> age) - (him >>> age))
  }

  def differences[Repr[_], Obs[_]](implicit
      O: Optica[Repr, Obs],
      M: Model[Repr]): Obs[Couples => List[(String, Int)]] = 
    differencesFl.getAll
}

