type t = Expat.xml_parser

type name = string
type attribute = name * name
type attributes = attribute list

type element =
  | Element of name * attributes * element list
  | Cdata of string


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


let add_subelement e stack =
  if Stack.length stack > 0 then
    let el = Stack.pop stack in
      match el with
	| Element (name, attrs, els) ->
	    Stack.push (Element (name, attrs, e::els)) stack
	| Cdata _ -> ()


let elstart st name attrs =
  Stack.push (Element (name, attrs, [])) st.stack;
  if Stack.length st.stack <= st.depth then
    st.start_callback name attrs


let elend st _name =
  let el = Stack.pop st.stack in
      match el with
	| Element (name, attrs, els) ->
	    let newel = Element (name, attrs, List.rev els) in
	      if Stack.length st.stack > st.depth then
		add_subelement newel st.stack
	      else
		if Stack.length st.stack == st.depth then
		  st.element_callback newel
		else
		  st.end_callback name
	| Cdata _ -> ()

let elcdata st cdata =
  add_subelement (Cdata cdata) st.stack

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

let rec element_to_string el =
  match el with
    | Element (name, attrs, els) ->
	if List.length els > 0 then
	  (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ ">" ^
	  (List.fold_left (^) "" (List.map element_to_string els)) ^
	  (Printf.sprintf "</%s>" name)
	else
	  (Printf.sprintf "<%s" name) ^ (attrs_to_string attrs) ^ "/>"
    | Cdata chunk -> crypt chunk





(****************************************************************************)

let is_element el =
  match el with
    | Element _ -> true
    | Cdata _ -> false


let get_cdata el =
  match el with
    | Element (_name, _attrs, els) -> (
	let append_chunk s el =
	  match el with
	    | Element _ -> s
	    | Cdata chunk -> s ^ chunk
	in
	  List.fold_left append_chunk "" els
      )
    | Cdata s -> s
