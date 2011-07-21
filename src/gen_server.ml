open Process

let section = Jamler_log.new_section "gen_server"

type system_msg = [ `System ]
type msg = [ system_msg | `Timeout ]
type reason = [ `Normal | `Exception of exn ]
type ('a, 'b) result =
    [ `Continue of 'a
    | `ContinueTimeout of 'a * float
    | `Stop of 'a
    | `StopReason of 'a * 'b
    ] Lwt.t

module type Type =
sig
  type msg
  type state
  type init_data
  type stop_reason
  val init : init_data -> msg pid -> (state, stop_reason) result
  val handle : msg -> state -> (state, stop_reason) result
  val terminate : state -> stop_reason -> unit Lwt.t
end

module type S =
sig
  type msg
  type init_data
  val start : init_data -> msg pid
end

module Make (T : Type with type msg = private [> msg]
		      and type stop_reason = private [> reason ]) :
sig
  type msg = [ `System | `Timeout ]
  type init_data = T.init_data
  val start : init_data -> T.msg pid
end =
struct
  type msg = [ `System | `Timeout ]
  type init_data = T.init_data
  let start init_data =
    let rec loop self state timeout =
      if is_overloaded self then (
	lwt () =
          Lwt_log.error ~section
	    "gen_server overloaded"
	in
          T.terminate state `Normal
      ) else (
	lwt msg =
	  match timeout with
	    | None -> receive self
	    | Some timeout ->
		Lwt.pick [receive self;
			  (Lwt_unix.sleep timeout >> Lwt.return `Timeout)]
	in
          match msg with
	    | #system_msg ->
		loop self state timeout
	    | m ->
		lwt result =
	          try_lwt
		    T.handle m state
		  with
		    | exn ->
			lwt () =
                          Lwt_log.error ~exn ~section
			    "gen_server raised an exception"
			in
                          Lwt.return (`StopReason (state, `Exception exn))
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
	       lwt result = T.init init_data self in
		 process_result self result)
end
