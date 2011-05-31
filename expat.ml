type xml_parser

external parser_create : string -> xml_parser = "mlXML_ParserCreate"
external parser_free : xml_parser -> unit = "mlXML_ParserFree"
external parse1 : xml_parser -> string -> bool -> bool = "mlXML_Parse"
external set_start_element_handler :
  xml_parser -> (string -> (string * string) list -> unit) -> unit =
    "mlXML_SetStartElementHandler"
external set_end_element_handler :
  xml_parser -> (string -> unit) -> unit = "mlXML_SetEndElementHandler"
external set_character_data_handler :
  xml_parser -> (string -> unit) -> unit = "mlXML_SetCharacterDataHandler"
external get_error_code : xml_parser -> int = "mlXML_GetErrorCode"
external error_string : int -> string = "mlXML_ErrorString"



exception Parse_error of string

let parse p s is_final =
  if not (parse1 p s is_final) then
    raise (Parse_error (error_string (get_error_code p)))

let print_attr a =
  match a with
      name, value -> Printf.printf "\t%s = %s\n" name value

let elstart name attrs =
  Printf.printf "start tag `%s'\n" name;
  List.iter print_attr attrs

let elend name =
  Printf.printf "end tag   `%s'\n" name

let elcdata cdata =
  Printf.printf "cdata     `%s'\n" cdata

(*
let p = parser_create "UTF-8"

let _ =
  set_start_element_handler p elstart;
  set_end_element_handler p elend;
  set_character_data_handler p elcdata;
  parse p "
<iq type='set' id='ft_1' to='joe@blow.com/Home'>
  <query xmlns='jabber:iq:filexfer'>
    <file 
            id='a0' 
            name='myfile.txt' 
            size='1024' 
            date='20020412T00:00:00'
	    hash='23e4ad6b63343b33a333c334'>
      A cool file
    </file>
  </query>
</iq>
" false

*)
