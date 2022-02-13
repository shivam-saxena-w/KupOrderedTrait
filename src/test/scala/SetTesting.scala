import org.scalatest.funsuite.AnyFunSuite

class SetTesting extends AnyFunSuite {
  val emptySetObj = new EmptySet[number]

  test("To check if an EmptySet is including a number"){
    val struct = emptySetObj.include(number(7)).include(number(3))
    assert(struct.contains(number(3)))
  }

  val nonEmptySetObj = new NonEmptySet[number](
    number(3),
    emptySetObj.include(number(2)),
    emptySetObj.include(number(1))
  )

  test("To check if an NonEmptySet is contains a number"){
    assert(nonEmptySetObj.contains(number(3)))
  }

  test("To check if an NonEmptySet does not contains a number"){
    assert(!nonEmptySetObj.contains(number(6)))
  }

}

