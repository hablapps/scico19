package optica
package triplet

import Function.const
import scalaz._, Scalaz._
import monocle.Optional, monocle.function.Index

case class ITree[I: Order, A](
    tag: A, 
    forest: I ==>> ITree[I, A] = ==>>.empty[I, ITree[I, A]]) {

  def merge(f: (A, A) => A, other: ITree[I, A]): ITree[I, A] =
    ITree(f(tag, other.tag), forest.unionWith(other.forest)(_.merge(f, _)))

  def lmerge(other: ITree[I, A]): ITree[I, A] =
    merge((l, _) => l, other)

  def map[B](f: A => B): ITree[I, B] =
    ITree(f(tag), forest.map(_.map(f)))

  def subtree(p: ITree[I, A] => Boolean): Option[ITree[I, A]] =
    if (p(this))
      Some(this) 
    else 
      forest.values.map(_.subtree(p)).find(_.isDefined).join
}

object ITree {

  implicit def iTreeFunctor[I: Order]: Functor[ITree[I, ?]] = 
      new Functor[ITree[I, ?]] {
    def map[A, B](fa: ITree[I, A])(f: A => B): ITree[I, B] = fa match {
      case ITree(tag, forest) => ITree(f(tag), forest.map(map(_)(f)))
    }
  }

  implicit def iTreeIndex[I: Order, A]: Index[ITree[I, A], I, ITree[I, A]] = 
      new Index[ITree[I, A], I, ITree[I, A]] { 
    def index(i: I) = Optional[ITree[I, A], ITree[I, A]](
      s => s.forest.lookup(i))(
      it => s => s.copy(forest = s.forest.adjust(i, const(it))))
  }

  implicit def iListTreeIndex[I: Order, A]: Index[ITree[I, A], List[I], ITree[I, A]] =
      new Index[ITree[I, A], List[I], ITree[I, A]] {
    def index(is: List[I]) = is.foldRight(Optional.id[ITree[I, A]]){ (i, acc) => 
      acc.composeOptional(iTreeIndex[I, A].index(i))
    }
  }
}

