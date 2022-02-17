type pid
type proc
type msg = ..

val pid_to_proc : pid -> proc
val proc_to_pid : proc -> pid
val spawn : (pid -> unit Lwt.t) -> pid
(*val send : pid -> msg -> unit*)
val ( $! ) : pid -> msg -> unit
val receive : pid -> msg Lwt.t

type msg += Erl of Erlang.erl_term

val register : pid -> string -> unit
val unregister : string -> unit
val whereis : string -> pid
val send_by_name : string -> msg -> unit
val ( $!! ) : string -> msg -> unit

val dist_send_ref : (Erlang.pid -> Erlang.erl_term -> unit) ref
val ( $!!! ) : Erlang.pid -> Erlang.erl_term -> unit

val dist_send_by_name_ref : (string -> string -> Erlang.erl_term -> unit) ref
val dist_send_by_name : string -> string -> Erlang.erl_term -> unit

module Pid :
sig
  type t = pid
  val equal : t -> t -> bool
  val hash : t -> int
end

type msg +=
   | Node_up of string
   | Node_down of string

val monitor_nodes : pid -> bool -> unit
val monitor_nodes_iter : (pid -> unit) -> unit

val is_overloaded : pid -> bool
val pid_to_string : pid -> string
val format_pid : unit -> pid -> string

type timer = unit Lwt.t
type msg += TimerTimeout of timer * msg
val send_after : float -> pid -> msg -> timer
val apply_after : float -> (unit -> unit Lwt.t) -> timer
val start_timer : float -> pid -> msg -> timer
val cancel_timer : timer -> unit
