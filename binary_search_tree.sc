
sealed trait Tree[+T] {
  def left: Tree[T]

  def right: Tree[T]

  def value: T
}


case class Branch[+T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]

case object Empty extends Tree[Nothing] {
  override def left: Tree[Nothing] = throw new Exception("empty")

  override def right: Tree[Nothing] = throw new Exception("empty")

  override def value: Nothing = throw new Exception("empty")
}

def find[T](v: T, tree: Tree[T])(implicit ordering: Ordering[T]): Tree[T] = tree match {
  case Empty => throw new Exception(s"value $v not found in tree")
  case branch@Branch(left, value, right) =>
    if (ordering.equiv(value, v)) branch
    else if (ordering.lt(v, value)) find(v, left)
    else find(v, right)
}

def insert[T](v: T, tree: Tree[T])(implicit ordering: Ordering[T]): Tree[T] = tree match {
  case Empty => Branch(Empty, v, Empty)
  case Branch(left, value, right) =>
    if (ordering.lt(v, value)) Branch(insert(v, left), value, right)
    else if (ordering.gt(v, value)) Branch(left, value, insert(v, right))
    else tree
}

def min[T](tree: Tree[T])(implicit ordering: Ordering[T]): T = {

  def loop(min: T, t: Tree[T]): T = (min, t) match {
    case (min, Empty) => min
    case (min, Branch(l, v, r)) => loop(v, l)
  }

  tree match {
    case Empty => throw new Exception("minimum of empty tree")
    case _ => loop(tree.value, tree)
  }

}

def remove[T](v: T, tree: Tree[T])(implicit ordering: Ordering[T]): Tree[T] = tree match {

  case branch@Branch(l, value, r) =>
    if (ordering.equiv(v, value)) (l, r) match {
      case (Empty, Empty) => Empty //Leaf, remove (replace with Empty)
      case (l, Empty) => l
      case (Empty, r) => r
      case (l, r) => {
        val successor = min(r)
        Branch(r.left, successor, remove(successor, r))
      }
    }
    else if (ordering.lt(v, tree.value)) branch.copy(left = remove(v, l))
    else branch.copy(right = remove(v, r))
  case Empty => throw new Exception("Value not present in tree")

}