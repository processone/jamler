open Process

let section = Jamler_log.new_section "gen_server"

type msg = [ `System ]
type 'a result =
    [ `Continue of 'a
    | `Stop of 'a
    ] Lwt.t

module type Type =
sig
  type msg
  type state
  type init_data
  val init : init_data -> msg pid -> state Lwt.t
  val handle : msg -> state -> state result
  val terminate : state -> unit Lwt.t
end

module type S =
sig
  type msg
  type init_data
  val start : init_data -> msg pid
end

module Make (T : Type with type msg = private [> msg]) :
sig
  type msg = [ `System ]
  type init_data = T.init_data
  val start : init_data -> T.msg pid
end =
struct
  type msg = [ `System ]
  type init_data = T.init_data
  let start init_data =
    let rec loop self state =
      if is_overloaded self then (
	lwt () =
          Lwt_log.error ~section
	    "gen_server overloaded"
	in
          T.terminate state
      ) else (
	lwt msg = receive self in
          match msg with
	    | #msg ->
		loop self state
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
                          Lwt.return (`Stop state)
		in
		  match result with
		    | `Continue state ->
			loop self state
		    | `Stop state ->
			T.terminate state
      )
    in
      spawn (fun self ->
	       lwt state = T.init init_data self in
		 loop self state)
end
