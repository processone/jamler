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
       `IQ {iq with
           Jlib.iq_type =
             `Error (Jlib.err_not_allowed, Some subel)}
    | `Get (`XmlElement ("ping", _, _)) ->
       `IQ {iq with Jlib.iq_type = `Result None}
    | `Get subel ->
       `IQ {iq with
           Jlib.iq_type =
             `Error (Jlib.err_service_unavailable, Some subel)}

  let start host =
    Mod_disco.register_feature host [%xmlns "PING"];
    [Gen_mod.iq_handler `Local host [%xmlns "PING"] iq_ping ();
     Gen_mod.iq_handler `SM host [%xmlns "PING"] iq_ping ();
    ]

  let stop host =
    Mod_disco.unregister_feature host [%xmlns "PING"];
    ()

end

let () = Gen_mod.register_mod (module ModPing : Gen_mod.Module)
