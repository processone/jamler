open Process

let src = Jamler_log.new_src "gen_server"

type reason = [ `Normal | `Exception of exn ]
type ('a, 'b) plain_result =
    [ `Continue of 'a
    | `ContinueTimeout of 'a * float
    | `Stop of 'a
    | `StopReason of 'a * 'b
    ]
type ('a, 'b) init_result = [ ('a, 'b) plain_result | `Init_failed ]
type ('a, 'b) result = ('a, 'b) plain_result

type msg +=
   (*| System*)
   | Timeout

module type Type =
sig
  type state
  type init_data
  type stop_reason
  val init : init_data -> pid -> (state, stop_reason) init_result
  val handle : Process.msg -> state -> (state, stop_reason) result
  val terminate : state -> stop_reason -> unit
end

module type S =
sig
  type init_data
  val start : init_data -> pid
end

module Make (T : Type with type stop_reason = private [> reason ]) :
sig
  type init_data = T.init_data
  val start : init_data -> pid
end = struct
  type init_data = T.init_data
  let start init_data =
    let rec loop self state timeout =
      if is_overloaded self then (
        Logs.err ~src
	  (fun m ->
            m "gen_server overloaded");
        T.terminate state `Normal
      ) else (
	let msg =
	  match timeout with
	  | None -> receive self
	  | Some timeout ->
             Eio.Fiber.first
               (fun () -> receive self)
               (fun () ->
                 Eio.Time.sleep (Process.get_global_env ())#clock timeout;
                 Timeout
               )
	in
        match msg with
	(*| System ->
	  loop self state timeout*)
	| m ->
	   let result =
	     try
	       T.handle m state
	     with
	     | exn ->
                Logs.err ~src
	          (fun m ->
                    m "gen_server raised an exception: %a"
                      Jamler_log.pp_exn exn);
                `StopReason (state, `Exception exn)
	   in
	   process_result self result
      )
    and process_result self =
      function
      | `Continue state ->
	 loop self state None
      | `ContinueTimeout (state, timeout) ->
	 loop self state (Some timeout)
      | `Stop state ->
	 T.terminate state `Normal
      | `StopReason (state, reason) ->
	 T.terminate state reason
    in
    spawn (fun self ->
	let result = T.init init_data self in
	match result with
	| `Init_failed -> ()
	| #plain_result as result ->
           process_result self result
      )
end
