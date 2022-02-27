package automata

class FiniteAutomataSuite extends munit.FunSuite:
  import finite.FA._

  trait TestFA:
    val dfaOne = DFA(
      states = Set(0),
      sigma = Set('a'),
      transition = (state, sym) => 0,
      initial = 0,
      f = Set(0)
    )

    val dfaTwo = DFA(
      states = Set(0),
      sigma = Set('a'),
      transition = (state, sym) => 0,
      initial = 0,
      f = Set()
    )

    val dfaThree = DFA(
      states = Set(1, 2, 3),
      sigma = Set('1', '0'),
      transition = (state, sym) =>
        (state, sym) match {
          case (1, '0') => 1
          case (1, '1') => 2
          case (2, '0') => 1
          case (2, '1') => 3
          case (3, '0') => 3
          case (3, '1') => 3
        },
      initial = 1,
      f = Set(3)
    )

  test("Empty DFA with initial as final always accepts") {
    new TestFA:
      assertEquals(dfaOne.accepts("a"), true)
  }

  test("Empty DFA with initial state as not final should always reject") {
    new TestFA:
      assertEquals(dfaTwo.accepts("aaa"), false)
  }

  test("DFA of A = { x | x contains substring 11 } should recognize A") {
    new TestFA:
      assertEquals(dfaThree.accepts("001100"), true)
      assertEquals(dfaThree.accepts("11"), true)
      assertEquals(dfaThree.accepts("001001"), false)
  }

  test("DFA responsds to invalid input") {
    new TestFA:
      assertEquals(dfaThree.accepts("01102"), false)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
