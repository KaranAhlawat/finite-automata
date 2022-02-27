package automata.finite

import scala.annotation.tailrec

object FA:
  type State = Int
  type Alphabet = Char
  case class DFA(
      private val states: Set[State],
      private val sigma: Set[Alphabet],
      private val transition: (State, Alphabet) => State,
      private val initial: State,
      private val f: Set[State]
  ):

    require(
      states.contains(initial),
      "Initial state should be an element of Q (set of all states)"
    )
    require(f.subsetOf(states), "F should be a subset of Q")

    def accepts(string: String): Boolean =
      @tailrec
      def recur(state: State, string: String): Boolean =
        if string.isEmpty then f.contains(state)
        else if sigma.contains(string.head) then
          recur(transition(state, string.head), string.tail)
        else false
      recur(initial, string)
