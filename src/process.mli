type -'a pid
type 'a proc = {			(* TODO: make abstract *)
  id : int;
  queue : 'a Queue.t;
  mutable t : unit Lwt.t;
  mutable overloaded : bool;
  mutable wakener : 'a Lwt.u option;
  mutable name : string option;
  mutable monitor_nodes : bool;
}
(*
external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"
*)
val pid_to_proc : 'a pid -> 'a proc
val proc_to_pid : 'a proc -> 'a pid
val spawn : ('a pid -> unit Lwt.t) -> 'a pid
(*val send : 'a pid -> 'a -> unit*)
val ( $! ) : 'a pid -> 'a -> unit
val receive : 'a pid -> 'a Lwt.t

type univ_msg = [ `Erl of Erlang.erl_term ]

val register : univ_msg pid -> string -> unit
val unregister : string -> unit
val whereis : string -> univ_msg pid
val send_by_name : string -> univ_msg -> unit
val ( $!! ) : string -> univ_msg -> unit

val dist_send_ref : (Erlang.pid -> Erlang.erl_term -> unit) ref
val ( $!!! ) : Erlang.pid -> Erlang.erl_term -> unit

type monitor_nodes_msg =
    [ `Node_up of string
    | `Node_down of string
    ]
val monitor_nodes : monitor_nodes_msg pid -> bool -> unit
val monitor_nodes_iter : (monitor_nodes_msg pid -> unit) -> unit

val is_overloaded : 'a pid -> bool
val pid_to_string : 'a pid -> string
val format_pid : unit -> 'a pid -> string

type empty
val any_pid : 'a pid -> empty pid

type timer = unit Lwt.t
type 'a timer_msg = [ `TimerTimeout of timer * 'a ]
val send_after : float -> 'a pid -> 'a -> timer
val apply_after : float -> (unit -> unit Lwt.t) -> timer
val start_timer : float -> 'a timer_msg pid -> 'a -> timer
val cancel_timer : timer -> unit
