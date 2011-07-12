let section = Jamler_log.new_section "process"

type -'a pid

type 'a proc = {id : int;
		queue : 'a Queue.t;
		mutable t : unit Lwt.t;
		mutable overloaded : bool;
		mutable wakener : 'a Lwt.u option}

external pid_to_proc : 'a pid -> 'a proc = "%identity"
external proc_to_pid : 'a proc -> 'a pid = "%identity"

let id_seq = ref 0

let spawn f =
  let proc = {id = (incr id_seq; !id_seq);
	      queue = Queue.create ();
	      t = Lwt.return ();
	      overloaded = false;
	      wakener = None}
  in
  let pid = proc_to_pid proc in
  let t =
    try_lwt
      f pid
    with
      | exn ->
	  lwt () =
            Lwt_log.error ~exn ~section "process raised an exception:"
	  in
            Lwt.fail exn
  in
    Lwt.on_cancel t (fun () -> Printf.printf "qwe\n%!");
    proc.t <- t;
    pid

exception Queue_limit

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
	lwt msg = waiter in
          proc.wakener <- None;
          Lwt.return msg
    ) else (
      Lwt.return (Queue.take proc.queue)
    )

let is_overloaded pid =
  let proc = pid_to_proc pid in
    proc.overloaded

type timer = unit Lwt.t
type 'a timer_msg = [ `TimerTimeout of timer * 'a ]

let send_after timeout pid msg =
  Lwt_unix.sleep timeout >>
    (pid $! msg;
     Lwt.return ())

let start_timer timeout pid msg =
  let t0 = Lwt.return () in
  let timer = ref t0 in
  let t =
    Lwt_unix.sleep timeout >>
      (pid $! `TimerTimeout (!timer, msg);
       Lwt.return ())
  in
    match Lwt.state t with
      | Lwt.Sleep ->
	  timer := t;
	  t
      | Lwt.Return () ->
	  t0
      | Lwt.Fail _ -> assert false

let cancel_timer timer = Lwt.cancel timer

