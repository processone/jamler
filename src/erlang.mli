type reference
type pid
type erl_term =
    ErlInt of int
  | ErlFloat of float
  | ErlAtom of string
  | ErlReference of reference
  | ErlPid of pid
  | ErlTuple of erl_term array
  | ErlNil
  | ErlString of string
  | ErlCons of erl_term * erl_term
  | ErlBinary of string
val term_to_string : erl_term -> string
val term_to_binary : erl_term -> string
val binary_to_term : string -> int -> erl_term * int
