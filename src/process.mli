type -'a pid
type 'a proc = {			(* TODO: make abstract *)
  id : int;
  queue : 'a Queue.t;
  mutable t : unit Lwt.t;
  mutable overloaded : bool;
  mutable wakener : 'a Lwt.u option;
}
external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"
val spawn : ('a pid -> unit Lwt.t) -> 'a pid
(*val send : 'a pid -> 'a -> unit*)
val ( $! ) : 'a pid -> 'a -> unit
val receive : 'a pid -> 'a Lwt.t
val is_overloaded : 'a pid -> bool

type timer = unit Lwt.t
type 'a timer_msg = [ `TimerTimeout of timer * 'a ]
val send_after : float -> 'a pid -> 'a -> timer
val apply_after : float -> (unit -> unit Lwt.t) -> timer
val start_timer : float -> 'a timer_msg pid -> 'a -> timer
val cancel_timer : timer -> unit
