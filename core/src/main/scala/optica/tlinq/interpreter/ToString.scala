package optica
package tlinq
package interpreter

trait ToStringTlinq extends Tlinq[λ[x => Int => String]] {

  def int(i: Int) = _ => s"$i"

  def bool(b: Boolean) = _ => s"$b"

  def string(s: String) = _ => s""""$s""""

  def foreach[A, B](
      as: Int => String)(
      f: (Int => String) => (Int => String)) = { i =>
    s"""for (x$i <- ${as(i)}) ${f(_ => s"x$i")(i + 1)}"""
  }

  def where[A](c: Int => String)(as: Int => String) = { i =>
    s"where ${c(i)} ${as(i)}"
  }

  def yields[A](a: Int => String) = { i =>
    s"yield ${a(i)}"
  }

  def product[A, B](a: Int => String, b: Int => String) = { i =>
    s"(${a(i)}, ${b(i)})"
  }

  def nil[A] = _ => "Nil"

  def subtract(x: Int => String, y: Int => String) = { i =>
    s"${x(i)} - ${y(i)}"
  }

  def greaterThan(x: Int => String, y: Int => String) = { i =>
    s"${x(i)} > ${y(i)}"
  }

  def equal[A](a1: Int => String, a2: Int => String) = { i =>
    s"${a1(i)} == ${a2(i)}"
  }

  def and(p: Int => String, q: Int => String) = { i =>
    s"${p(i)} && ${q(i)}"
  }

  def not(b: Int => String) = { i =>
    s"!${b(i)}"
  }

  def exists[A](as: Int => String) = { i =>
    s"exists ${as(i)}"
  }

  def ifs[A](c: Int => String, t: Int => String, e: Int => String) = { i =>
    s"if (${c(i)}) ${t(i)} else ${e(i)}"
  }

  def lam[A, B](f: (Int => String) => (Int => String)) = { i =>
    s"""(λx$i -> ${f(_ => s"x$i")(i + 1)})"""
  }

  def app[A, B](f: Int => String)(a: Int => String) = { i =>
    s"${f(i)}(${a(i)})"
  }

  def some[A](a: Int => String) = { i =>
    s"Some(${a(i)})"
  }

  def none[A] = _ => "None"

  def ofold[A, B](oa: Int => String)(z: Int => String, f: Int => String) = { i =>
    s"${oa(i)}.fold(${z(i)})(${f(i)})"
  }
}

class ToString extends ToStringTlinq

