
trait Set[A <: Ordered[A]]{
  def contains(value: A): Boolean
  def include(value: A): Set[A]
}

class EmptySet[A <: Ordered[A]] extends Set[A]{
  override def contains(value: A): Boolean = false
  override def include(value: A): Set[A] = new NonEmptySet(value, new EmptySet[A], new EmptySet[A])
}

class NonEmptySet[A <: Ordered[A]](value: A, leftChild: Set[A], rightChild: Set[A]) extends Set[A]{
  override def contains(value: A): Boolean = {
    if (value > this.value) rightChild contains value
    else if (value < this.value) leftChild contains value
    else true
  }

  override def include(value: A): Set[A] = {
    if value < this.value then new NonEmptySet[A](this.value, leftChild include value, rightChild)
    else if value > this.value  then new NonEmptySet[A](this.value, leftChild, rightChild include value)
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