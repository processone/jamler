open Process

type xml_msg =
    [ `XmlStreamStart of Xml.name * Xml.attribute list
    | `XmlStreamElement of Xml.element
    | `XmlStreamEnd of Xml.name
    | `XmlStreamError of string
    ]

type msg += Xml of xml_msg

type t = {pid : pid;
	  mutable xml_parser : Xml.t;
          queue : msg list ref}

let create pid =
  let queue = ref [] in
  let element_callback el =
    queue := Xml (`XmlStreamElement el) :: !queue
  in
  let start_callback name attrs =
    queue := Xml (`XmlStreamStart (name, attrs)) :: !queue
  in
  let end_callback name =
    queue := Xml (`XmlStreamEnd name) :: !queue
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
     xml_parser;
     queue}

let parse st data =
  try
    Xml.parse st.xml_parser data false;
    List.iter (fun msg -> st.pid $! msg) (List.rev !(st.queue));
    st.queue := []
  with
  | Expat.Parse_error error ->
     st.pid $! Xml (`XmlStreamError error)

let free st =
  Expat.parser_free st.xml_parser

let reset_stream st =
  Expat.parser_free st.xml_parser;
  let queue = st.queue in
  let element_callback el =
    queue := Xml (`XmlStreamElement el) :: !queue
  in
  let start_callback name attrs =
    queue := Xml (`XmlStreamStart (name, attrs)) :: !queue
  in
  let end_callback name =
    queue := Xml (`XmlStreamEnd name) :: !queue
  in
  let xml_parser =
    Xml.create_parser
      ~depth:1
      ~element_callback
      ~start_callback
      ~end_callback
      ()
  in
  st.xml_parser <- xml_parser;
  queue := []

