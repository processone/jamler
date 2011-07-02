let section = Jamler_log.new_section "config"

let myhosts () =
  List.map Jlib.nameprep_exn
    ["localhost"; "e.localhost"; "hoi.jabber.ru"; "zinid.ru"] (* TODO *)

let auth_modules _host =
  ["sql"]

let modules _host =
  [("mod_roster_sql", []);
   ("mod_version", []);
  ]

let process_config config =
  lwt () = Lwt_log.notice_f ~section "config is fine" in
    Lwt.return ()

let read_config filename =
  lwt () = Lwt_log.notice_f ~section
    "using config file \"%s\"" filename in
  try
    lwt fd = Lwt_unix.openfile filename [Unix.O_RDONLY] 0o640 in
    lwt stats = Lwt_unix.fstat fd in
    let buf = String.create stats.Unix.st_size in
    lwt res = Lwt_unix.read fd buf 0 stats.Unix.st_size in
    lwt () = Lwt_unix.close fd in
    let config = Cfg.config_of_string buf in
    let _ = process_config config in
      Lwt.return ()
  with
    | exn ->
	lwt () = Lwt_log.error_f ~section
	  "Failed to process config: %s\n" (Printexc.to_string exn)
	in Lwt.return ()
