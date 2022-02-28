package automata.finite

class FiniteAutomataSuite extends munit.FunSuite:
  import automata.finite.FiniteAutomata

  trait TestFA:
    object dfaOne extends FiniteAutomata[Int, Char]:
      override val states: States = List(0)
      override val sigma: Set[Alphabet] = Set('a')
      override val initial: State = 0
      override val accept: States = List(0)
      override val transition: (State, Alphabet) => State = (state, sym) => 0

    object dfaTwo extends FiniteAutomata[Int, Char]:
      override val states: States = List(0)
      override val sigma: Set[Alphabet] = Set('a')
      override val initial: State = 0
      override val accept: States = Nil
      override val transition: (State, Alphabet) => State = (state, sym) => 0

    object dfaThree extends FiniteAutomata[Int, Char]:
      override val states: States = List(1, 2, 3)
      override val sigma: Set[Alphabet] = Set('0', '1')
      override val initial: State = 1
      override val accept: States = List(3)
      override val transition: (State, Alphabet) => State = (state, sym) =>
        (state, sym) match {
          case (1, '0') => 1
          case (1, '1') => 2
          case (2, '0') => 1
          case (2, '1') => 3
          case (3, '0') => 3
          case (3, '1') => 3
        }

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

  test("DFA responds to invalid input") {
    new TestFA:
      assertEquals(dfaThree.accepts("01102"), false)
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
