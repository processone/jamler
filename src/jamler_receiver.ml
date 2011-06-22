open Process

type msg =
    [ `XmlStreamStart of Xml.name * Xml.attribute list
    | `XmlStreamElement of Xml.element
    | `XmlStreamEnd of Xml.name
    | `XmlStreamError of string
    ]

type t = {pid : msg pid;
	  mutable xml_parser : Xml.t}

let create pid =
  let pid = (pid :> msg pid) in
  let element_callback el =
    pid $! `XmlStreamElement el
  in
  let start_callback name attrs =
    pid $! `XmlStreamStart (name, attrs)
  in
  let end_callback name =
    pid $! `XmlStreamEnd name
  in
  let xml_parser =
    Xml.create_parser
      ~depth:1
      ~element_callback
      ~start_callback
      ~end_callback
      ()
  in
    {pid;
     xml_parser}

let parse st data =
  try
    Xml.parse st.xml_parser data false
  with
    | Expat.Parse_error error ->
	st.pid $! `XmlStreamError error

let free st =
  Expat.parser_free st.xml_parser

let reset_stream st =
  Expat.parser_free st.xml_parser;
  let pid = st.pid in
  let element_callback el =
    pid $! `XmlStreamElement el
  in
  let start_callback name attrs =
    pid $! `XmlStreamStart (name, attrs)
  in
  let end_callback name =
    pid $! `XmlStreamEnd name
  in
  let xml_parser =
    Xml.create_parser
      ~depth:1
      ~element_callback
      ~start_callback
      ~end_callback
      ()
  in
    st.xml_parser <- xml_parser

