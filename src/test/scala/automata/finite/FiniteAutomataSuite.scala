package automata.finite

class FiniteAutomataSuite extends munit.FunSuite:
  import automata.finite.FiniteAutomata._

  trait TestFA:
    object dfaOne extends DFA[Int, Char]:
      override protected val states: States = List(0)
      override protected val sigma: Set[Alphabet] = Set('a')
      override protected val initial: State = 0
      override protected val accept: States = List(0)
      override def transition(state: State, alphabet: Alphabet): Option[State] =
        Some(0)

    object dfaTwo extends DFA[Int, Char]:
      override protected val states: States = List(0)
      override protected val sigma: Set[Alphabet] = Set('a')
      override protected val initial: State = 0
      override protected val accept: States = Nil
      override def transition(state: State, alphabet: Alphabet): Option[State] =
        Some(0)

    object dfaThree extends DFA[Int, Char]:
      override protected val states: States = List(1, 2, 3)
      override protected val sigma: Set[Alphabet] = Set('0', '1')
      override protected val initial: State = 1
      override protected val accept: States = List(3)
      override def transition(state: State, alphabet: Alphabet): Option[State] =
        (state, alphabet) match {
          case (1, '0') => Some(1)
          case (1, '1') => Some(2)
          case (2, '0') => Some(1)
          case (2, '1') => Some(3)
          case (3, '0') => Some(3)
          case (3, '1') => Some(3)
          case _        => None
        }

  test("Empty DFA with initial as final always accepts") {
    new TestFA:
      assertEquals(dfaOne.accepts("a"), true)
  }

  test("Empty DFA with initial state as not final should always reject") {
    new TestFA:
      assertEquals(dfaTwo.accepts("aaa"), false)
      assertEquals(dfaTwo.accepts(""), false)
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
