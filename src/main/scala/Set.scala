
trait Set[A <: Ordered[A]]{
  def contains(value: A): Boolean
  def include(nodeValue: A): Set[A]
}

class EmptySet[A <: Ordered[A]] extends Set[A]{
  override def contains(value: A): Boolean = false
  override def include(nodeValue: A): Set[A] = new NonEmptySet(nodeValue, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <: Ordered[A]](nodeValue: A, leftChild: Set[A], rightChild: Set[A]) extends Set[A]{
  override def contains(value: A): Boolean = {
    if (value > nodeValue) rightChild contains nodeValue
    else if (value < nodeValue) leftChild contains nodeValue
    else true
  }

  override def include(value: A): Set[A] = {
    if value < nodeValue then new NonEmptySet[A](nodeValue, leftChild include value, rightChild)
    else if value > nodeValue  then new NonEmptySet[A](value, leftChild, rightChild include nodeValue)
    else this
  }

}

case class number(element: Int) extends Ordered[number] {
  override def compare(that: number): Int = {
    if that.element > this.element then 1
    else if that.element < this.element then -1
    else 0
  }
}