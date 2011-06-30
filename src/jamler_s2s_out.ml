open Process

let rec loop self =
  lwt _ = receive self in
  loop self

let start _ _ _ =
  spawn loop

let start_connection _ = ()
let stop_connection _ = ()
