module GenIQHandler = Jamler_gen_iq_handler

module ModPing :
sig
  include Gen_mod.Module
end
  =
struct
  let name = "mod_ping"

  let iq_ping _from _to iq =
    match iq.Jlib.iq_type with
      | `Set subel ->
	  Lwt.return (`IQ {iq with
                             Jlib.iq_type =
                          `Error (Jlib.err_not_allowed, Some subel)})
      | `Get (`XmlElement ("ping", _, _)) ->
	  Lwt.return (`IQ {iq with Jlib.iq_type = `Result None})
      | `Get subel ->
	  Lwt.return (`IQ {iq with
                             Jlib.iq_type =
                          `Error (Jlib.err_service_unavailable, Some subel)})

  let start host =
    Mod_disco.register_feature host [%ns:PING];
    Lwt.return (
      [Gen_mod.iq_handler `Local host [%ns:PING] iq_ping ();
       Gen_mod.iq_handler `SM host [%ns:PING] iq_ping ();
      ]
    )

  let stop host =
    Mod_disco.unregister_feature host [%ns:PING];
    Lwt.return ()

end

let () = Gen_mod.register_mod (module ModPing : Gen_mod.Module)
