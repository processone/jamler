let section = Jamler_log.new_section "process"

type msg = ..

type pid = int

type proc = {id : int;
	     queue : msg Queue.t;
	     mutable t : unit Lwt.t;
	     mutable overloaded : bool;
	     mutable wakener : msg Lwt.u option;
	     mutable name : string option;
	     mutable monitor_nodes : bool;
	    }

let max_processes = 65536

let processes : proc option array = Array.make max_processes None

let free_pids = Array.init (max_processes + 1) (fun i -> i)
let free_pid_start = ref 0
let free_pid_end = ref max_processes

exception Process_table_full

let add_free_pid pid =
  free_pids.(!free_pid_end) <- pid;
  incr free_pid_end;
  if !free_pid_end >= max_processes + 1
  then free_pid_end := 0;
  assert (!free_pid_start <> !free_pid_end)

let get_free_pid () =
  if !free_pid_start = !free_pid_end
  then raise Process_table_full;
  let pid = free_pids.(!free_pid_start) in
    incr free_pid_start;
    if !free_pid_start >= max_processes + 1
    then free_pid_start := 0;
    pid


type msg += Erl of Erlang.erl_term

let registered : (string, pid) Hashtbl.t = Hashtbl.create 10

(*
external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"
*)

let pid_to_proc (pid : pid) =
  match processes.(pid) with
  | Some proc -> proc
  | None -> raise Not_found

let proc_to_pid : proc -> pid =
  fun proc -> proc.id

let spawn f =
  let id = get_free_pid () in
  let proc = {id;
	      queue = Queue.create ();
	      t = Lwt.return ();
	      overloaded = false;
	      wakener = None;
	      name = None;
	      monitor_nodes = false;
	     }
  in
  let () = processes.(id) <- Some proc in
  let pid = proc_to_pid proc in
  let t =
    try%lwt
      Lwt.finalize (fun () -> f pid)
	(fun () ->
	   processes.(id) <- None;
	   add_free_pid id;
	   (match proc.name with
	      | None -> ()
	      | Some name ->
		  Hashtbl.remove registered name
	   );
	   Lwt.return ()
	)
    with
      | exn ->
	  let%lwt () =
            Lwt_log.error ~exn ~section "process raised an exception:"
	  in
            Lwt.fail exn
  in
    Lwt.on_cancel t (fun () -> Printf.printf "qwe\n%!");
    proc.t <- t;
    pid

(*exception Queue_limit*)

let send pid msg =
  let proc = pid_to_proc pid in
    (match proc.wakener with
       | None ->
	   if Queue.length proc.queue > 10000
	   then (
	     proc.overloaded <- true;
	     Lwt.cancel proc.t
	   ) else Queue.add msg proc.queue
       | Some wakener ->
	   Lwt.wakeup wakener msg
    )

let ($!) = send

let receive pid =
  let proc = pid_to_proc pid in
    if Queue.is_empty proc.queue then (
      let (waiter, wakener) = Lwt.wait () in
	proc.wakener <- Some wakener;
	let%lwt msg = waiter in
          proc.wakener <- None;
          Lwt.return msg
    ) else (
      Lwt.return (Queue.take proc.queue)
    )

let register pid name =
  let proc = pid_to_proc pid in
    if Hashtbl.mem registered name then (
      invalid_arg "name is already in use"
    ) else (
      match proc.name with
	| Some _ ->
	    invalid_arg "already registered"
	| None ->
	    proc.name <- Some name;
	    Hashtbl.replace registered name pid
    )

let unregister name =
  if Hashtbl.mem registered name then (
    let pid = Hashtbl.find registered name in
    let proc = pid_to_proc pid in
      proc.name <- None;
      Hashtbl.remove registered name
  ) else invalid_arg "not a registered name"

let whereis name =
  if Hashtbl.mem registered name then (
    Hashtbl.find registered name
  ) else invalid_arg "not a registered name"

let send_by_name name msg =
  whereis name $! msg

let ($!!) = send_by_name

let dist_send_ref : (Erlang.pid -> Erlang.erl_term -> unit) ref =
  ref (fun _pid _term -> ())

let ($!!!) pid term = !dist_send_ref pid term

let dist_send_by_name_ref : (string -> string -> Erlang.erl_term -> unit) ref =
  ref (fun _name _node _term -> ())

let dist_send_by_name name node term = !dist_send_by_name_ref name node term

type msg +=
   | Node_up of string
   | Node_down of string

let monitor_nodes_pids : (pid, unit) Hashtbl.t =
  Hashtbl.create 10

let monitor_nodes (pid : pid) flag =
  let proc = pid_to_proc pid in
    if proc.monitor_nodes <> flag then (
      proc.monitor_nodes <- flag;
      if flag then (
	Hashtbl.replace monitor_nodes_pids pid ()
      ) else (
	Hashtbl.remove monitor_nodes_pids pid
      )
    )

let monitor_nodes_iter f =
  Hashtbl.iter (fun pid () -> f pid) monitor_nodes_pids

let is_overloaded pid =
  let proc = pid_to_proc pid in
    proc.overloaded

let pid_to_string pid =
  "<" ^ string_of_int pid ^ ">"

let format_pid () pid = pid_to_string pid

module Pid :
sig
  type t = pid
  val equal : t -> t -> bool
  val hash : t -> int
end
  =
struct
  type t = pid
  let equal p1 p2 = p1 = p2
  let hash p =
    Hashtbl.hash p
end

type timer = unit Lwt.t
type msg += TimerTimeout of timer * msg

let send_after timeout pid msg =
  let%lwt () = Lwt_unix.sleep timeout in
  pid $! msg;
  Lwt.return ()

let apply_after timeout f =
  let%lwt () = Lwt_unix.sleep timeout in
  let%lwt () = f () in
  Lwt.return ()

let start_timer timeout pid msg =
  let t0 = Lwt.return () in
  let timer = ref t0 in
  let t =
    let%lwt () = Lwt_unix.sleep timeout in
    pid $! TimerTimeout (!timer, msg);
    Lwt.return ()
  in
    match Lwt.state t with
      | Lwt.Sleep ->
	  timer := t;
	  t
      | Lwt.Return () ->
	  t0
      | Lwt.Fail _ -> assert false

let cancel_timer timer = Lwt.cancel timer

