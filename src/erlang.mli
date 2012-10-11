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

val node_of_pid : pid -> string
val make_pid : string -> int -> int -> pid

val term_to_string : erl_term -> string
val term_to_buffer : Buffer.t -> erl_term -> unit
val term_to_binary : erl_term -> string
val binary_to_term : string -> int -> erl_term * int

val jid_to_term : Jlib.jid -> erl_term
val term_to_route : erl_term -> [> `Route of Jlib.jid * Jlib.jid * Xml.element ]

module ErlType :
sig
  type 'a repr
  val int : int repr
  val string : string repr
  val binary : string repr
  val atom : string repr
  val xml : Xml.element repr
  val ( * ) : 'a repr -> 'b repr -> ('a * 'b) repr
  val list : 'a repr -> 'a list repr

  val from_term : 'a repr -> erl_term -> 'a
  val to_term : 'a repr -> 'a -> erl_term
end

