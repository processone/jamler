type t = Expat.xml_parser

type name = string
type attribute = name * name
type attributes = attribute list

type element_cdata =
  [ `XmlElement of name * attributes * element_cdata list
  | `XmlCdata of string ]

type element =
  [ `XmlElement of name * attributes * element_cdata list ]


type parser_state = {stack : element Stack.t;
		     depth  : int;
		     element_callback : element -> unit;
		     start_callback : name -> attribute list -> unit;
		     end_callback : name -> unit;
		    }

let get_attr_exn = List.assoc 

let get_attr name attrs =
  try
    Some (get_attr_exn name attrs)
  with
    | Not_found -> None

let get_attr_s name attrs =
  try
    List.assoc name attrs
  with
    | Not_found -> ""

let get_tag_attr_exn name (`XmlElement (_name, attrs, _els)) =
  get_attr_exn name attrs
let get_tag_attr name (`XmlElement (_name, attrs, _els)) =
  get_attr name attrs
let get_tag_attr_s name (`XmlElement (_name, attrs, _els)) =
  get_attr_s name attrs

let add_subelement e stack =
  if Stack.length stack > 0 then
    let el = Stack.pop stack in
      match el with
	| `XmlElement (name, attrs, els) ->
	    Stack.push (`XmlElement (name, attrs, e::els)) stack
	| `XmlCdata _ -> ()


let elstart st name attrs =
  Stack.push (`XmlElement (name, attrs, [])) st.stack;
  if Stack.length st.stack <= st.depth then
    st.start_callback name attrs


let elend st _name =
  let el = Stack.pop st.stack in
  let `XmlElement (name, attrs, els) = el in
  let newel = `XmlElement (name, attrs, List.rev els) in
    if Stack.length st.stack > st.depth
    then add_subelement newel st.stack
    else if Stack.length st.stack == st.depth
    then st.element_callback newel
    else st.end_callback name

let elcdata st cdata =
  add_subelement (`XmlCdata cdata) st.stack

let create_parser ?(encoding = "UTF-8") ~depth
    ~element_callback ~start_callback ~end_callback () =
  let p = Expat.parser_create encoding
  and st = {stack = Stack.create ();
	    depth = depth;
	    element_callback = element_callback;
	    start_callback = start_callback;
	    end_callback = end_callback;
	   } in
    Expat.set_start_element_handler p (elstart st);
    Expat.set_end_element_handler p (elend st);
    Expat.set_character_data_handler p (elcdata st);
    p

let parse = Expat.parse

let parse_element s =
  let res = ref None in
  let element_callback el =
    res := Some el
  in
  let p =
    create_parser
      ~depth:0
      ~element_callback
      ~start_callback:(fun _ _ -> assert false)
      ~end_callback:(fun _ -> assert false)
      ()
  in
    try
      parse p s true;
      Expat.parser_free p;
      (match !res with
	 | Some res -> res
	 | None -> assert false
      )
    with
      | exn ->
	  Expat.parser_free p;
	  raise exn


let crypt' b s =
  let l = String.length s in
    for i = 0 to l - 1 do
      match s.[i] with
	| '&' -> Buffer.add_string b "&amp;"
	| '<' -> Buffer.add_string b "&lt;"
	| '>' -> Buffer.add_string b "&gt;"
	| '\"' -> Buffer.add_string b "&quot;"
	| '\'' -> Buffer.add_string b "&apos;"
	| c -> Buffer.add_char b c
    done

let attrs_to_string' b attrs =
  let attr_to_string (name, value) =
    Buffer.add_char b ' ';
    Buffer.add_string b name;
    Buffer.add_string b "='";
    crypt' b value;
    Buffer.add_char b '\'';
  in
    List.iter attr_to_string attrs

let rec element_cdata_to_string' b el =
  match el with
  | `XmlElement (name, attrs, []) ->
     Buffer.add_char b '<';
     Buffer.add_string b name;
     attrs_to_string' b attrs;
     Buffer.add_string b "/>"
  | `XmlElement (name, attrs, els) ->
     Buffer.add_char b '<';
     Buffer.add_string b name;
     attrs_to_string' b attrs;
     Buffer.add_char b '>';
     element_cdata_to_string'_els b els;
     Buffer.add_string b "</";
     Buffer.add_string b name;
     Buffer.add_char b '>';
  | `XmlCdata chunk ->
     crypt' b chunk
and element_cdata_to_string'_els b els =
  match els with
  | [] -> ()
  | el :: els ->
     element_cdata_to_string' b el;
     element_cdata_to_string'_els b els

let crypt s =
  let l = String.length s in
  let b = Buffer.create l in
    crypt' b s;
    Buffer.contents b

let attrs_to_string attrs =
  let b = Buffer.create 10 in
    attrs_to_string' b attrs;
    Buffer.contents b

let element_cdata_to_string el =
  let b = Buffer.create 256 in
    element_cdata_to_string' b el;
    Buffer.contents b

let element_to_string el =
  element_cdata_to_string (el :> element_cdata)



(****************************************************************************)

let is_element el =
  match el with
    | `XmlElement _ -> true
    | `XmlCdata _ -> false

(*
let get_cdata el =
  match el with
    | `XmlElement (_name, _attrs, els) -> (
	let append_chunk s el =
	  match el with
	    | `XmlElement _ -> s
	    | `XmlCdata chunk -> s ^ chunk
	in
	  List.fold_left append_chunk "" els
      )
    | `XmlCdata s -> s
*)

let rec get_cdata' =
  function
    | `XmlElement _ :: els -> get_cdata' els
    | `XmlCdata s :: els -> s :: get_cdata' els
    | [] -> []

let get_cdata els = String.concat "" (get_cdata' els)

let get_tag_cdata (`XmlElement (_name, _attrs, els)) =
    get_cdata els

let rec remove_cdata =
  function
    | `XmlElement _ as el :: els -> el :: remove_cdata els
    | `XmlCdata _ :: els -> remove_cdata els
    | [] -> []


let replace_tag_attr attr value (`XmlElement (name, attrs, els)) =
  let attrs = List.remove_assoc attr attrs in
  let attrs = (attr, value) :: attrs in
    `XmlElement (name, attrs, els)

let get_subtag (`XmlElement (_, _, els)) name =
  let rec get_subtag' els name =
    match els with
      | el :: els -> (
	  match el with
	    | `XmlElement (name', _, _) as el when name = name' ->
		Some el
	    | _ ->
		get_subtag' els name
	)
      | [] ->
	  None
  in
    get_subtag' els name



(* TODO: redesign *)
let rec get_path_s el =
  function
    | [] -> assert false
    | `Elem name :: path -> (
	match get_subtag el name with
	  | None -> ""
	  | Some sub_el ->
	      get_path_s sub_el path
      )
    | [`Attr name] ->
	get_tag_attr_s name el
    | [`Cdata] ->
	get_tag_cdata el
    | _ -> assert false

