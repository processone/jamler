(*let new_section = Lwt_log.Section.make

let _ =
  let logger =
    Lwt_log.channel
      ~template:"$(date).$(milliseconds): $(section)($(level)): $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout
      ()
  in
    Lwt_log.default := logger
 *)

let new_src = Logs.Src.create 

let pp_exn ppf exn =
  let bt = Printexc.get_raw_backtrace () in
  Fmt.exn_backtrace ppf (exn, bt)

let reporter ppf =
  let report src level ~over k msgf =
    let construct ?header ?tags:_ fmt =
      let k _ = k () in
      let t = Ptime_clock.now () in
      let tz = Ptime_clock.current_tz_offset_s () in
      let src_name = Logs.Src.name src in
      let res =
        Format.kfprintf k ppf ("%a %s%a: " ^^ fmt ^^ "@.")
          (Ptime.pp_rfc3339 ?tz_offset_s:tz ~space:true ~frac_s:3 ()) t
          src_name
          Logs_fmt.pp_header (level, header)
      in
      over ();
      res
    in
    msgf @@ construct
  in
  { Logs.report = report }

let _ =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (reporter (Format.std_formatter));
  Logs.set_level (Some Logs.Info)
