package automata.finite

import scala.annotation.tailrec

trait FiniteAutomata[T, A]():
  type Alphabet = A
  type State = T
  type States = Seq[State]

  val states: States
  val sigma: Set[Alphabet]
  val initial: State
  val accept: States
  val transition: (State, Alphabet) => State

  def accepts(string: Seq[Alphabet]): Boolean =
    @tailrec
    def recur(state: State, string: Seq[Alphabet]): Boolean =
      if string.isEmpty then accept.contains(state)
      else if !sigma.contains(string.head) then false
      else recur(transition(state, string.head), string.tail)
    recur(initial, string)