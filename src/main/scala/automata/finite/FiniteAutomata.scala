package automata.finite

import scala.annotation.tailrec

object FiniteAutomata:
  trait DFA[T, A]:
    type Alphabet = A
    type State = T
    type States = Seq[State]

    protected val states: States
    protected val sigma: Set[Alphabet]
    protected val initial: State
    protected val accept: States
    def transition(state: State, alphabet: Alphabet): Option[State]

    def accepts(string: Seq[Alphabet]): Boolean =
      @tailrec
      def recur(state: State, string: Seq[Alphabet]): Boolean =
        if string.isEmpty then accept.contains(state)
        else if !sigma.contains(string.head) then false
        else
          transition(state, string.head) match {
            case Some(next) => recur(next, string.tail)
            case None       => false
          }
      recur(initial, string)
