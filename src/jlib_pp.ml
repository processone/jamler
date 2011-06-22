open Camlp4.PreCast

let namespaces =
  [("DISCO_ITEMS", "http://jabber.org/protocol/disco#items");
   ("DISCO_INFO", "http://jabber.org/protocol/disco#info");
   ("VCARD", "vcard-temp");
   ("VCARD_UPDATE", "vcard-temp:x:update");
   ("AUTH", "jabber:iq:auth");
   ("AUTH_ERROR", "jabber:iq:auth:error");
   ("REGISTER", "jabber:iq:register");
   ("SEARCH", "jabber:iq:search");
   ("ROSTER", "jabber:iq:roster");
   ("ROSTER_VER", "urn:xmpp:features:rosterver");
   ("PRIVACY", "jabber:iq:privacy");
   ("BLOCKING", "urn:xmpp:blocking");
   ("PRIVATE", "jabber:iq:private");
   ("VERSION", "jabber:iq:version");
   ("TIME90", "jabber:iq:time"); (* TODO: Remove once XEP-0090 is Obsolete *)
   ("TIME", "urn:xmpp:time");
   ("LAST", "jabber:iq:last");
   ("XDATA", "jabber:x:data");
   ("IQDATA", "jabber:iq:data");
   ("DELAY91", "jabber:x:delay"); (* TODO: Remove once XEP-0091 is Obsolete *)
   ("DELAY", "urn:xmpp:delay");
   ("EXPIRE", "jabber:x:expire");
   ("EVENT", "jabber:x:event");
   ("CHATSTATES", "http://jabber.org/protocol/chatstates");
   ("XCONFERENCE", "jabber:x:conference");
   ("STATS", "http://jabber.org/protocol/stats");
   ("MUC", "http://jabber.org/protocol/muc");
   ("MUC_USER", "http://jabber.org/protocol/muc#user");
   ("MUC_ADMIN", "http://jabber.org/protocol/muc#admin");
   ("MUC_OWNER", "http://jabber.org/protocol/muc#owner");
   ("MUC_UNIQUE", "http://jabber.org/protocol/muc#unique");
   ("PUBSUB", "http://jabber.org/protocol/pubsub");
   ("PUBSUB_EVENT", "http://jabber.org/protocol/pubsub#event");
   ("PUBSUB_OWNER", "http://jabber.org/protocol/pubsub#owner");
   ("PUBSUB_NMI", "http://jabber.org/protocol/pubsub#node-meta-info");
   ("PUBSUB_ERRORS", "http://jabber.org/protocol/pubsub#errors");
   ("PUBSUB_NODE_CONFIG", "http://jabber.org/protocol/pubsub#node_config");
   ("PUBSUB_SUB_OPTIONS", "http://jabber.org/protocol/pubsub#subscribe_options");
   ("PUBSUB_SUB_AUTH", "http://jabber.org/protocol/pubsub#subscribe_authorization");
   ("PUBSUB_GET_PENDING", "http://jabber.org/protocol/pubsub#get-pending");
   ("COMMANDS", "http://jabber.org/protocol/commands");
   ("BYTESTREAMS", "http://jabber.org/protocol/bytestreams");
   ("ADMIN", "http://jabber.org/protocol/admin");
   ("SERVERINFO", "http://jabber.org/network/serverinfo");

   ("RSM", "http://jabber.org/protocol/rsm");
   ("EJABBERD_CONFIG", "ejabberd:config");

   ("STREAM", "http://etherx.jabber.org/streams");
   ("FLASH_STREAM", "http://www.jabber.com/streams/flash");

   ("STANZAS", "urn:ietf:params:xml:ns:xmpp-stanzas");
   ("STREAMS", "urn:ietf:params:xml:ns:xmpp-streams");

   ("TLS", "urn:ietf:params:xml:ns:xmpp-tls");
   ("SASL", "urn:ietf:params:xml:ns:xmpp-sasl");
   ("SESSION", "urn:ietf:params:xml:ns:xmpp-session");
   ("BIND", "urn:ietf:params:xml:ns:xmpp-bind");

   ("FEATURE_IQAUTH", "http://jabber.org/features/iq-auth");
   ("FEATURE_IQREGISTER", "http://jabber.org/features/iq-register");
   ("FEATURE_COMPRESS", "http://jabber.org/features/compress");
   ("FEATURE_MSGOFFLINE", "msgoffline");

   ("COMPRESS", "http://jabber.org/protocol/compress");

   ("CAPS", "http://jabber.org/protocol/caps");
   ("SHIM", "http://jabber.org/protocol/shim");
   ("ADDRESS", "http://jabber.org/protocol/address");

   (* CAPTCHA related NSes. *)
   ("OOB", "jabber:x:oob");
   ("CAPTCHA", "urn:xmpp:captcha");
   ("MEDIA", "urn:xmpp:media-element");
   ("BOB", "urn:xmpp:bob");
  ]

let expr_quotation_expander _loc _loc_name_opt str =
  let value = List.assoc str namespaces in
    <:expr< $str:value$ >>

let patt_quotation_expander _loc _loc_name_opt str =
  let value = List.assoc str namespaces in
    <:patt< $str:value$ >>

let str_item_quotation_expander _loc _loc_name_opt str =
  <:str_item< $exp: expr_quotation_expander _loc _loc_name_opt str$ >>

let _ =
  Syntax.Quotation.add "ns" Syntax.Quotation.DynAst.expr_tag
    expr_quotation_expander;
  Syntax.Quotation.add "ns" Syntax.Quotation.DynAst.patt_tag
    patt_quotation_expander;
  Syntax.Quotation.add "ns" Syntax.Quotation.DynAst.str_item_tag
    str_item_quotation_expander
