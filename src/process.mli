type -'a pid
type 'a proc = {			(* TODO: make abstract *)
  id : int;
  queue : 'a Queue.t;
  mutable wakener : 'a Lwt.u option;
}
external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"
val spawn : ('a pid -> 'b Lwt.t) -> 'a pid
(*val send : 'a pid -> 'a -> unit*)
val ( $! ) : 'a pid -> 'a -> unit
val receive : 'a pid -> 'a Lwt.t
