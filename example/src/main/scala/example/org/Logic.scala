package example
package org

import optica._
import concrete._
import symantics._

import Optica.syntax._

object Logic {

  def expertiseFl[Repr[_]](
      u: String)(implicit
      O: Optica_[Repr],
      M: Model[Repr]): Repr[Fold[Org, String]] = {
    import O._, M._
    departments >>> filtered(employees.all((tasks >>> tsk).elem(u))) >>> dpt
  }

  def expertise[Repr[_], Obs[_]](
      u: String)(implicit
      O: Optica[Repr, Obs],
      M: Model[Repr]): Obs[Org => List[String]] =
    expertiseFl(u).getAll
}

