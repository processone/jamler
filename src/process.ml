let src = Jamler_log.new_src "process"

let (global_switch : Eio.Switch.t option ref) = ref None

let set_global_switch sw = global_switch := Some sw
let get_global_switch () = Option.get !global_switch

let (global_env : Eio_unix.Stdenv.base option ref) = ref None

let set_global_env sw = global_env := Some sw
let get_global_env () = Option.get !global_env

type msg = ..

type pid = int

type proc = {id : int;
	     queue : msg Queue.t;
	     mutable t : Eio.Cancel.t option;
	     mutable overloaded : bool;
	     mutable wakener : msg Eio.Promise.u option;
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
	      t = None;
	      overloaded = false;
	      wakener = None;
	      name = None;
	      monitor_nodes = false;
	     }
  in
  let () = processes.(id) <- Some proc in
  let pid = proc_to_pid proc in
  Eio.Fiber.fork
    ~sw:(get_global_switch ())
    (fun () ->
      try
        Fun.protect
          (fun () ->
            Eio.Cancel.sub
              (fun cancel ->
                proc.t <- Some cancel;
                f pid
              )
          )
	  ~finally:(fun () ->
	    processes.(id) <- None;
	    add_free_pid id;
	    (match proc.name with
	     | None -> ()
	     | Some name ->
		Hashtbl.remove registered name
	    )
	  )
      with
      | exn ->
         Logs.err ~src
	   (fun m ->
             m "process raised an exception: %a"
               Jamler_log.pp_exn exn);
         raise exn
    );
  (*Lwt.on_cancel t (fun () -> Printf.printf "qwe\n%!");*)
  pid

exception Queue_limit

let send pid msg =
  match pid_to_proc pid with
  | proc -> (
    match proc.wakener with
    | None ->
       if Queue.length proc.queue > 10000
       then (
	 proc.overloaded <- true;
	 Option.iter (fun t -> Eio.Cancel.cancel t Queue_limit) proc.t;
       ) else Queue.add msg proc.queue
    | Some wakener ->
       proc.wakener <- None;
       Eio.Promise.resolve wakener msg
  )
  | exception Not_found -> ()

let ($!) = send

let receive pid =
  let proc = pid_to_proc pid in
  if Queue.is_empty proc.queue then (
    let (waiter, wakener) = Eio.Promise.create () in
    proc.wakener <- Some wakener;
    let msg = Eio.Promise.await waiter in
    msg
  ) else (
    Queue.take proc.queue
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
let pp_pid = Fmt.of_to_string pid_to_string

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

type timer = Eio.Cancel.t
type msg += TimerTimeout of timer * msg

let apply_after timeout f =
  let cancel = ref None in
  Eio.Fiber.fork
    ~sw:(get_global_switch ())
    (fun () ->
      try
        Eio.Cancel.sub
          (fun c ->
            cancel := Some c;
            Eio.Time.sleep (get_global_env ())#clock timeout;
            f ();
          )
      with
      | Eio.Cancel.Cancelled _ -> ()
    );
  match !cancel with
  | None -> assert false
  | Some cancel -> cancel

let send_after timeout pid msg =
  apply_after timeout (fun () -> pid $! msg)

let start_timer timeout pid msg =
  let cancel = ref None in
  Eio.Fiber.fork
    ~sw:(get_global_switch ())
    (fun () ->
      try
        Eio.Cancel.sub
          (fun c ->
            cancel := Some c;
            Eio.Time.sleep (get_global_env ())#clock timeout;
            pid $! TimerTimeout (c, msg);
          )
      with
      | Eio.Cancel.Cancelled _ -> ()
    );
  match !cancel with
  | None -> assert false
  | Some cancel -> cancel

exception Cancel

let cancel_timer timer = Eio.Cancel.cancel timer Cancel
