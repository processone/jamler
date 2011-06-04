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

let crypt s =
  let l = String.length s in
  let b = Buffer.create l in
    for i = 0 to l - 1 do
      match s.[i] with
	| '&' -> Buffer.add_string b "&amp;"
	| '<' -> Buffer.add_string b "&lt;"
	| '>' -> Buffer.add_string b "&gt;"
	| '\"' -> Buffer.add_string b "&quot;"
	| '\'' -> Buffer.add_string b "&apos;"
	| c -> Buffer.add_char b c
    done;
    Buffer.contents b

let rec attrs_to_string attrs =
  let attr_to_string attr =
    match attr with
      | (name, value) -> " " ^ name ^ "='" ^ (crypt value) ^ "'"
  in List.fold_left (^) "" (List.map attr_to_string attrs)

let rec element_cdata_to_string el =
  match el with
    | `XmlElement (name, attrs, els) ->
	if List.length els > 0 then
	  (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ ">" ^
	  (List.fold_left (^) "" (List.map element_cdata_to_string els)) ^
	  (Printf.sprintf "</%s>" name)
	else
	  (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ "/>"
    | `XmlCdata chunk -> crypt chunk

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

let rec remove_cdata =
  function
    | `XmlElement _ as el :: els -> el :: remove_cdata els
    | `XmlCdata _ :: els -> remove_cdata els
    | [] -> []


let replace_tag_attr attr value (`XmlElement (name, attrs, els)) =
  let attrs = List.remove_assoc attr attrs in
  let attrs = (attr, value) :: attrs in
    `XmlElement (name, attrs, els)

