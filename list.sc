import scala.annotation.tailrec

sealed trait List[+T] {
  def head: T
  def tail: List[T]
  def isEmpty: Boolean
}


case class Cons[T](head: T, tail: List[T]) extends List[T]{
  def isEmpty = false
}


case object Nil extends List[Nothing] {
  def isEmpty = true
  def head = throw new Exception("head of empty list")
  def tail = throw new Exception("tail of empty list")
}
//O(1)
def prepend[T](t:T, l:List[T]) = Cons(t,l)
//O(n)
def append[T](t: T,l: List[T]): List[T] =
	if(l.isEmpty) Cons(t, Nil)
	else Cons(l.head, append(t,l.tail))


def get[T](n: Int, l: List[T]): T = (n,l) match {
	case (_,Nil) => throw new Exception("index out of bounds")
	case (0,Cons(head,_)) => head
	case (n,Cons(_,tail)) => get(n-1, tail)
}


def concat[T](a:List[T], b: List[T]): List[T] = (a,b) match {
  case (Nil,b) => b
  case (a,Nil) => a
  case (a@Cons(x,xs),Cons(y,ys)) => concat(append(y,a), ys)
}

def last[T](a: List[T]): T = a match {
  case Nil => throw new Exception("Empty list")
  case Cons(x,Nil) => x
  case Cons(x,xs) =>  last(xs)
}

//O(n*n) append * reverse
def reverse[T](a: List[T]): List[T] = a match {
  case Nil => Nil
  case a@Cons(x,Nil) => a
  case a@Cons(x,xs) => append(x,reverse(xs))
}

//O(n)  prepend * loop
def reverseTail[T](a: List[T]): List[T] = {
  @tailrec
    def loop(reversed: List[T], orig: List[T]): List[T] =
(reversed,orig) match {
case (reversed, Nil) => reversed
case (reversed, Cons(x,xs)) => loop(prepend(x, reversed), xs)
}
  loop(Nil, a)
}

reverseTail(Cons(5,Cons(4,Cons(3,Cons(2,Cons(1, Nil))))))
