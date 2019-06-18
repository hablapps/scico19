package example.couple
package interpreter

import optica.concrete._

class RModel extends Model[λ[x => x]] {

    val couples = Fold(identity)

    val her = Getter(_.her)

    val him = Getter(_.him)

    val name = Getter(_.name)

    val age = Getter(_.age)
}

