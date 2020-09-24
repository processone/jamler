open Ppxlib

type val_type =
  | Int
  | String
  | Bool

type state =
  {query : Buffer.t;
   mutable param_pos : int;
   params : expression;
   res_pos : int;
   res : expression list;
   res_vars : pattern;
  }

let rec parse st loc = function
  | '@' :: '(' :: s ->
     let (col, s) = parse_name s in
     let (t, s) = parse_type s in
     let st =
       Buffer.add_string st.query col;
       parse st loc s
     in
     let var = "__v" ^ string_of_int st.res_pos in
     let evar = Ast_builder.Default.evar ~loc var in
     let pvar = Ast_builder.Default.pvar ~loc var in
      let (r, v) =
	match t with
	  | Int ->
	      ([%expr int_of_string [%e evar]],
	       [%pat? Some [%p pvar]])
	  | String ->
	      (evar,
	       [%pat? Some [%p pvar]])
	  | Bool ->
	      ([%expr Sql.bool_of_string [%e evar]],
	       [%pat? Some [%p pvar]])
      in
	{st with
	   res_pos = st.res_pos + 1;
	   res = r :: st.res;
	   res_vars = [%pat? [%p v] :: [%p st.res_vars]];
	}
  | '%' :: '(' :: s ->
     let (var, s) = parse_name s in
     let (t, s) = parse_type s in
     let st =
       Buffer.add_string st.query ("$" ^ string_of_int st.param_pos);
       st.param_pos <- st.param_pos + 1;
       parse st loc s
     in
     let evar = Ast_builder.Default.evar ~loc var in
     let param =
       match t with
       | Int -> [%expr Some (string_of_int [%e evar])]
       | String -> [%expr Some [%e evar]]
       | Bool -> [%expr Some (Sql.string_of_bool [%e evar])]
     in
     {st with params = [%expr [%e param] :: [%e st.params]]}
  | c :: s ->
     Buffer.add_char st.query c;
     parse st loc s
  | [] -> st
and parse_name = function
  | ')' :: s -> ("", s)
  | c :: s ->
     let (name, s) = parse_name s in
     (String.make 1 c ^ name, s)
  | [] ->
     let err = Printf.sprintf "Unclosed '('" in
     raise (Invalid_argument err)
and parse_type = function
  | 'd' :: s -> (Int, s)
  | 's' :: s -> (String, s)
  | 'b' :: s -> (Bool, s)
  | c :: _ ->
     let err = Printf.sprintf "Invalid type specifier `%c'" c in
     raise (Invalid_argument err)
  | [] ->
     let err = Printf.sprintf "No type specifier" in
     raise (Invalid_argument err)

let list_of_string s =
  let cs = ref [] in
  String.iter (fun c -> cs := c :: !cs) s;
  List.rev !cs

let expr_expander ~ctxt str =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let st =
    {query = Buffer.create 10;
     param_pos = 1;
     params = [%expr []];
     res_pos = 1;
     res = [];
     res_vars = [%pat? []];
    }
  in
  let st = parse st loc (list_of_string str) in
  let query = Buffer.contents st.query in
  let equery = Ast_builder.Default.estring ~loc query in
  let res =
    match st.res with
      | [] -> [%expr ()]
      | [r] -> r
      | _ -> (
        match Ast_builder.Default.pexp_tuple_opt ~loc st.res with
        | Some res -> res
        | None -> assert false
      )
  in
    [%expr Sql.make_select_query [%e equery] [%e st.params]
      (function [%p st.res_vars] -> Some [%e res] | _ -> None)]

let sql_extension =
  Extension.V3.declare
    "sql"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expr_expander

let rule = Ppxlib.Context_free.Rule.extension sql_extension

let () =
 Driver.register_transformation
   ~rules:[rule]
   "sql"
