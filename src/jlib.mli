type namepreped = private string
type nodepreped = private string
type resourcepreped = private string

type jid = {user : string;
	    server : string;
	    resource : string;
	    luser : nodepreped;
	    lserver : namepreped;
	    lresource : resourcepreped;
	   }

val nameprep_exn : string -> namepreped
val nodeprep_exn : string -> nodepreped
val resourceprep_exn : string -> resourcepreped

val nameprep : string -> namepreped option
val nodeprep : string -> nodepreped option
val resourceprep : string -> resourcepreped option

exception Bad_jid

val make_jid_exn : string -> string -> string -> jid
val make_jid : string -> string -> string -> jid option
val make_jid' : nodepreped -> namepreped -> resourcepreped -> jid
val ljid_to_jid : nodepreped * namepreped * resourcepreped -> jid
val string_to_jid_exn : string -> jid
val string_to_jid : string -> jid option
val jid_to_string : jid -> string
val jid_to_string' : string -> string -> string -> string
val jid_tolower : jid -> nodepreped * namepreped * resourcepreped
val jid_remove_resource : jid -> jid
val jid_replace_resource' : jid -> resourcepreped -> jid

val err_bad_request : Xml.element
val err_conflict : Xml.element
val err_feature_not_implemented : Xml.element
val err_forbidden : Xml.element
val err_gone : Xml.element
val err_internal_server_error : Xml.element
val err_item_not_found : Xml.element
val err_jid_malformed : Xml.element
val err_not_acceptable : Xml.element
val err_not_allowed : Xml.element
val err_not_authorized : Xml.element
val err_payment_required : Xml.element
val err_recipient_unavailable : Xml.element
val err_redirect : Xml.element
val err_registration_required : Xml.element
val err_remote_server_not_found : Xml.element
val err_remote_server_timeout : Xml.element
val err_resource_constraint : Xml.element
val err_service_unavailable : Xml.element
val err_subscription_required : Xml.element
val err_unexpected_request : Xml.element
val err_unexpected_request_cancel : Xml.element

val errt_bad_request : string -> string -> Xml.element
val errt_conflict : string -> string -> Xml.element
val errt_feature_not_implemented : string -> string -> Xml.element
val errt_forbidden : string -> string -> Xml.element
val errt_gone : string -> string -> Xml.element
val errt_internal_server_error : string -> string -> Xml.element
val errt_item_not_found : string -> string -> Xml.element
val errt_jid_malformed : string -> string -> Xml.element
val errt_not_acceptable : string -> string -> Xml.element
val errt_not_allowed : string -> string -> Xml.element
val errt_not_authorized : string -> string -> Xml.element
val errt_payment_required : string -> string -> Xml.element
val errt_recipient_unavailable : string -> string -> Xml.element
val errt_redirect : string -> string -> Xml.element
val errt_registration_required : string -> string -> Xml.element
val errt_remote_server_not_found : string -> string -> Xml.element
val errt_remote_server_timeout : string -> string -> Xml.element
val errt_resource_constraint : string -> string -> Xml.element
val errt_service_unavailable : string -> string -> Xml.element
val errt_subscription_required : string -> string -> Xml.element
val errt_unexpected_request : string -> string -> Xml.element

(* Auth stanza errors *)
val err_auth_no_resource_provided : string -> Xml.element
val err_auth_bad_resource_format : string -> Xml.element
val err_auth_resource_conflict : string -> Xml.element

val serr_bad_format : Xml.element
val serr_bad_namespace_prefix : Xml.element
val serr_conflict : Xml.element
val serr_connection_timeout : Xml.element
val serr_host_gone : Xml.element
val serr_host_unknown : Xml.element
val serr_improper_addressing : Xml.element
val serr_internal_server_error : Xml.element
val serr_invalid_from : Xml.element
val serr_invalid_id : Xml.element
val serr_invalid_namespace : Xml.element
val serr_invalid_xml : Xml.element
val serr_not_authorized : Xml.element
val serr_policy_violation : Xml.element
val serr_remote_connection_failed : Xml.element
val serr_resourse_constraint : Xml.element
val serr_restricted_xml : Xml.element
val serr_see_other_host : string -> Xml.element
val serr_system_shutdown : Xml.element
val serr_unsupported_encoding : Xml.element
val serr_unsupported_stanza_type : Xml.element
val serr_unsupported_version : Xml.element
val serr_xml_not_well_formed : Xml.element

type iq_query =
    [ `Get of Xml.element
    | `Set of Xml.element ]
type iq_response =
    [ `Result of Xml.element option
    | `Error of Xml.element * Xml.element option ]
type iq_query_response = [ iq_query | iq_response ]

type +'a iq =
    {iq_id : string;
     iq_type : [< iq_query_response ] as 'a;
     iq_xmlns : string;
     iq_lang : string}

val iq_query_or_response_info : Xml.element ->
  [ `IQ of iq_query_response iq
  | `Invalid
  | `Not_iq
  | `Reply ]

val iq_query_info : Xml.element ->
  [ `IQ of iq_query iq
  | `Invalid
  | `Not_iq
  | `Reply ]

val make_result_iq_reply : Xml.element -> Xml.element
val make_error_reply : Xml.element -> Xml.element -> Xml.element

val replace_from_to_attrs :
  string -> string -> Xml.attributes -> Xml.attributes
val replace_from_to : jid -> jid -> Xml.element -> Xml.element
val remove_attr : string -> Xml.element -> Xml.element

val iq_to_xml : iq_query_response iq -> Xml.element

val sha1 : string -> string
val md5 : string -> string

val decode_base64 : string -> string
val encode_base64 : string -> string

val get_random_string : unit -> string

val timestamp_to_iso: Unix.tm -> string

module LJID :
sig
  type t = nodepreped * namepreped * resourcepreped
  val compare : t -> t -> int
  val to_string : t -> string
end

module LJIDSet :
sig
  include Set.S with type elt = LJID.t

  val from_list : LJID.t list -> t
end

