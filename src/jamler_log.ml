let new_section = Lwt_log.Section.make

let _ =
  let logger =
    Lwt_log.channel
      ~template:"$(date).$(milliseconds): $(section)($(level)): $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout
      ()
  in
    Lwt_log.default := logger
