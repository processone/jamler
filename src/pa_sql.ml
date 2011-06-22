open Camlp4.PreCast

type val_type =
  | Int
  | String
  | Bool

type state =
  {query : Buffer.t;
   mutable param_pos : int;
   params : Ast.expr;
   res_pos : int;
   res : Ast.expr list;
   res_vars : Ast.patt;
  }

let rec parse st _loc = parser
  | [< ''@'; ''('; col = parse_name; t = parse_type;
       st = (Buffer.add_string st.query col;
	     parse st _loc) >] ->
      let var = "__v" ^ string_of_int st.res_pos in
      let (r, v) =
	match t with
	  | Int ->
	      (<:expr< int_of_string $lid:var$ >>,
	       <:patt< Some $lid:var$ >>)
	  | String ->
	      (<:expr< $lid:var$ >>,
	       <:patt< Some $lid:var$ >>)
	  | Bool ->
	      (<:expr< Sql.bool_of_string $lid:var$ >>,
	       <:patt< Some $lid:var$ >>)
      in
	{st with
	   res_pos = st.res_pos + 1;
	   res = r :: st.res;
	   res_vars = <:patt< $v$ :: $st.res_vars$ >>;
	}
  | [< ''%'; ''('; var = parse_name; t = parse_type;
       st = (Buffer.add_string st.query ("$" ^ string_of_int st.param_pos);
	     st.param_pos <- st.param_pos + 1;
	     parse st _loc) >] ->
      let param =
	match t with
	  | Int -> <:expr< Some (string_of_int $lid:var$) >>
	  | String -> <:expr< Some $lid:var$ >>
	  | Bool -> <:expr< Some (Sql.string_of_bool $lid:var$) >>
      in
	{st with params = <:expr< $param$ :: $st.params$ >>}
  | [< 'c; st = (Buffer.add_char st.query c; parse st _loc) >] -> st
  | [< >] -> st
and parse_name = parser
  | [< '')' >] -> ""
  | [< 'c; s = parse_name >] -> String.make 1 c ^ s
and parse_type = parser
  | [< ''d' >] -> Int
  | [< ''s' >] -> String
  | [< ''b' >] -> Bool
  | [< 'c >] ->
      let err = Printf.sprintf "Invalid type specifier `%c'" c in
	raise (Invalid_argument err)


let expr_quotation_expander _loc _loc_name_opt str =
  let st =
    {query = Buffer.create 10;
     param_pos = 1;
     params = <:expr< [] >>;
     res_pos = 1;
     res = [];
     res_vars = <:patt< [] >>;
    }
  in
  let st = parse st _loc (Stream.of_string str) in
  let query = Buffer.contents st.query in
  let res =
    match st.res with
      | [] -> <:expr< () >>
      | [r] -> r
      | _ ->
	  let res = Ast.exCom_of_list st.res in
	  let res = <:expr< ($tup:res$) >> in
	    res
  in
    <:expr< Sql.make_select_query $str:query$ $st.params$
      (function $st.res_vars$ -> Some $res$ | _ -> None) >>

let str_item_quotation_expander _loc _loc_name_opt str =
  <:str_item< $exp: expr_quotation_expander _loc _loc_name_opt str$ >>

let _ =
  Syntax.Quotation.add "sql" Syntax.Quotation.DynAst.expr_tag
    expr_quotation_expander;
  Syntax.Quotation.add "sql" Syntax.Quotation.DynAst.str_item_tag
    str_item_quotation_expander
