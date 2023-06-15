(*
  Copyright 2023 Souchet Ferdinand

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
  documentation files (the “Software”), to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit
  persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
  OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)


type expr =
  | Var    of string
  | Apply  of expr * expr
  | Lambda of string * expr
  | Group  of expr (* TODO: remove groups *)
;;


type statement =
  | Lazy  of string * expr
  | Eager of string * expr
;;

exception UndefinedVariable of string;;

module VariableSet = Set.Make(String);;


let rec expr_raw_to_str exp =
  let is_lambda = function
    | Lambda(_, _) -> true
    | _            -> false
  in
  let is_apply = function
    | Apply(_, _) -> true
    | _           -> false
  in
  match exp with
  | Var(v)          -> v
  | Group(e)        -> "(" ^ expr_raw_to_str e ^ ")"
  | Lambda(v, e)    -> v ^ " -> " ^ expr_raw_to_str e
  | Apply(e1, e2)   -> 
    (
      if is_lambda e1 then
        expr_raw_to_str (Group(e1))
      else
        expr_raw_to_str e1
    ) ^ " "
    ^ (
      if is_lambda e2 || is_apply e2 then
        expr_raw_to_str (Group(e2))
      else
        expr_raw_to_str e2
    )
;;


exception Incorrect;;
let expr_to_str exp =
  let _bool = function
  | Lambda(a, Lambda(b, Var c)) -> string_of_bool (a = c)
  | _                           -> raise Incorrect
  in
  let _number exp =
    let rec _count exp f a count =
      match exp with
      | Var(v) when v = a           -> count
      | Apply(Var v, e) when v = f  -> _count e f a (count+1)
      | _                           -> raise Incorrect
    in
    match exp with
    | Lambda(f, Lambda(a, e)) when f <> a -> string_of_int @@ _count e f a 0
    | _                                   -> raise Incorrect
  in
  try
    _bool exp
  with Incorrect ->
  try _number exp
  with Incorrect ->
    expr_raw_to_str exp
;;


let print_expr exp =
  print_string @@ expr_to_str exp
;;


let print_raw_expr exp =
  print_string @@ expr_raw_to_str exp
;;


let print_statement = function
  | Lazy(name, expr)  -> Printf.printf "%s := %s" name (expr_raw_to_str expr)
  | Eager(name, expr) -> Printf.printf "%s = %s" name (expr_raw_to_str expr)
;;


(* Change all the (not free) variable var in exp to something that has not yet been defined *)
let rec alpha_convert exp var defined =
  let rec _new_undef_name curr =
    if VariableSet.mem curr defined then
      _new_undef_name (curr^"'")
    else curr
  in
  let rec _rename exp new_name defined =
    match exp with
    | Var(v) when v = var         -> Var(new_name)
    | Var(v)                      -> exp
    | Apply(e1, e2)               -> Apply(_rename e1 new_name defined, _rename e2 new_name defined)
    | Lambda(v, e) when v = var   -> alpha_convert exp var (VariableSet.add new_name defined)
    | Lambda(v, e)                -> Lambda(v, _rename e new_name (VariableSet.add v defined))
    | Group(e)                    -> Group(_rename e new_name defined)
  in
  match exp with
  | Apply(e1, e2)             -> Apply(alpha_convert e1 var defined, alpha_convert e2 var defined)
  | Lambda(v, e) when v = var -> let new_name = _new_undef_name (var^"'") in Lambda(new_name, _rename e new_name defined)
  | Lambda(v, e)              -> Lambda(v, alpha_convert e var (VariableSet.add v defined))
  | Group(e)                  -> Group(alpha_convert e var defined)
  | Var(v)                    -> exp
;;



let get_vars e =
  let rec _extract e def =
    match e with
    | Var(v)          -> VariableSet.add v def
    | Apply(e1, e2)   -> VariableSet.union (_extract e1 def) (_extract e2 def)
    | Lambda(var, e)  -> VariableSet.add var (_extract e def)
    | Group(e)        -> _extract e def
  in _extract e VariableSet.empty
;;


let free_var e =
  let rec _extract e def free =
    match e with
    | Var(v) -> ( if not @@ VariableSet.mem v def then VariableSet.add v free else free )
    | Apply(e1, e2) -> VariableSet.union (_extract e1 def free) (_extract e2 def VariableSet.empty)
    | Lambda(var, e) -> _extract e (VariableSet.add var def) free
    | Group(e) -> _extract e def free
  in _extract e VariableSet.empty VariableSet.empty
;;



let substitute dst var src defined =
  let rec _subst dst var src defined src_free =
    match dst with
    | Var(v) when v = var                               -> src
    | Var(v)                                            -> dst
    | Apply(e1, e2)                                     ->
      Apply(_subst e1 var src defined src_free, _subst e2 var src defined src_free)
    | Lambda(v, e) when v = var                         -> dst (* variable shadowing *)
    | Lambda(v, e)                                      ->
      let vars_e = VariableSet.remove v @@ get_vars e in
      let alpha_defined = VariableSet.union (VariableSet.union defined src_free) vars_e in
      if VariableSet.mem v alpha_defined then
        _subst (alpha_convert dst v alpha_defined) var src defined src_free
      else
        Lambda(v, _subst e var src (VariableSet.add v defined) src_free)
    | Group(e)                                          -> Group(_subst e var src defined src_free)
  in
  let free = free_var src in
  _subst dst var src defined free
;;


let eager_beta_reduce exp defined =
  let rec _reduce exp defined =
    (* print_expr exp; print_newline (); *)
    match exp with
    | Apply(Lambda(v, e), arg) ->
      let (rarg, _) = _reduce arg defined in
      let (r, _) = _reduce (substitute e v rarg defined) defined in (r, true)
    | Apply(e1, e2) ->
      let (re1, m1) = _reduce e1 defined in
      let (re2, m2) = _reduce e2 defined in
      if m1 || m2 then
        _reduce (Apply(re1, re2)) defined
      else
        (Apply(re1, re2), false)
    | Lambda(v, e) -> let (r, m) = _reduce e (VariableSet.add v defined) in (Lambda(v, r), m)
    | Group(e) -> _reduce e defined
    | _ -> (exp, false)
  in
  let (r, _) = _reduce exp defined in r
;;


let lazy_beta_reduce exp defined = 
  let rec _reduce exp defined =
    (* print_expr exp; print_newline (); *)
    match exp with
    | Apply(Lambda(v, e), arg) -> let (r, _) = _reduce (substitute e v arg defined) defined in (r, true)
    | Apply(e1, e2) ->
      let (re1, m1) = _reduce e1 defined in
      (* let (re2, m2) = _reduce e2 defined in *)
      if m1 then
        _reduce (Apply(re1, e2)) defined
      else (
        (* let (re2, m2) = _reduce e2 defined in
        (Apply(e1, re2), m2) *)
        (Apply(e1, e2), false)
      )
    | Lambda(v, e) -> let (r, m) = _reduce e (VariableSet.add v defined) in (Lambda(v, r), m)
    | Group(e) -> _reduce e defined
    | _ -> (exp, false)
  in
  let (r, _) = _reduce exp defined in r
;;


let beta_reduce = lazy_beta_reduce;;


(* let kestrel = Lambda("a", Lambda("b", Var("a")));;
let mockingbird = Lambda("a", Apply(Var "a", Var "a"));;
let idiot = Lambda("a", Var "a");;
let kite = beta_reduce (Apply(kestrel, idiot)) VariableSet.empty;;
let cardinal = Lambda("f", Lambda("a", Lambda("b", Apply(Apply(Var "f", Var "b"), Var "a"))));; *)


(* TODO: catch already defined var *)
class context = object (self)
  val mutable vars = Hashtbl.create 20
  val mutable cont = VariableSet.empty

  method exec (s: statement) =
    let (name, expr, b_red) = match s with
      | Lazy(n, e)  -> (n, e, lazy_beta_reduce)
      | Eager(n, e) -> (n, e, eager_beta_reduce)
    in
    let expr = ref @@ beta_reduce expr cont in
    let free = free_var !expr in

    (* Replace all instances of variables by their values*)
    VariableSet.iter (fun name -> 
      if VariableSet.mem name cont then (
        let value = Hashtbl.find vars name in
        expr := substitute !expr name value cont;
      ) else
        raise (UndefinedVariable name)
    ) free;

    (* Apply the right beta reduction *)
    expr := b_red !expr cont;
    if name <> "" then (
      cont <- VariableSet.add name cont;
      Hashtbl.add vars name !expr;
    );
    !expr
  
  method print () =
    Hashtbl.iter (fun name exp -> print_statement (Eager(name, exp)); print_newline ()) vars
end;;
