open Data_flow;;
open While_ast;;
open Utils;;

module IdentSet = Set.Make(struct type t = ident let compare = compare end);;
module LabelSet = Set.Make(struct type t = EBSet.key let compare = compare end);;

let free_variables_aexpr expr =
  let rec fv_aexpr expr s =
  match expr with
  | AConst _ -> s
  | AIdent x -> IdentSet.add x s
  | Op (_, a1, a2) -> let lh = fv_aexpr a1 s in fv_aexpr a2 lh
  in fv_aexpr expr IdentSet.empty;;

let free_variables_bexpr expr =
  let rec compute expr s =
    match expr with
    | BConst _ -> s
    | Neg nexpr -> compute nexpr s
    | RelOp (_, a1, a2) ->
        let lh = free_variables_aexpr a1 in 
        let rh = free_variables_aexpr a2 in
        IdentSet.union lh rh
    | BoolOp (_, b1, b2) ->
        let lh = compute b1 s in compute b2 lh
  in compute expr IdentSet.empty;;

let genc c =
  match c with
  | Right bexpr -> free_variables_bexpr bexpr
  | Left stmt ->
    (match stmt with
     | AssignStmt (_, aexpr, _) -> free_variables_aexpr aexpr
     | _ -> IdentSet.empty
    );;

let killc c =
  match c with
  | Right _ -> IdentSet.empty
  | Left stmt ->
      (match stmt with
       | AssignStmt (x, _, _) -> IdentSet.singleton x
       | _ -> IdentSet.empty);;

let gen dfn =
  match dfn with
  | { content = c ; children = _ } -> genc c;;

let kill dfn =
  match dfn with
  | { content = c ; children = _ } -> killc c;;

let final_set dfg max_lab = 
  List.fold_left (fun finalset lab ->
    let node = EBSet.find lab dfg in
      match List.filter (fun chl -> chl > lab) node.children with
      | [] -> LabelSet.add lab finalset
      | _ -> finalset) LabelSet.empty (range (max_lab - 1));;

module CIMap = Map.Make(struct type t = EBSet.key let compare = compare end);;

let start_point max_lab = 
  List.fold_left (fun m n -> CIMap.add n (IdentSet.empty, IdentSet.empty) m) CIMap.empty (range (max_lab - 1));;

let ci_converged prev_it cur_it =
  let (prev_entry, prev_exit) = prev_it and
      (cur_entry, cur_exit) = cur_it in
    IdentSet.equal prev_entry cur_entry && IdentSet.equal prev_exit cur_exit;;

let lv_entry exit node =
  IdentSet.union (IdentSet.diff exit (kill node)) (gen node);;

let lv_exit final_set n cmap node =
  if LabelSet.mem n final_set then 
    IdentSet.empty
  else 
    List.fold_left 
      (fun s l -> IdentSet.union s (fst (CIMap.find l cmap))) 
      IdentSet.empty node.children;;

let lv_iterate idfg final_set max_lab f cmap =
  let ncmap = List.fold_left (fun m n ->
    let (entry, exit) = CIMap.find n m in
    let node = EBSet.find n idfg in
    let nentry = lv_entry exit node in
    let nexit = lv_exit final_set n m node in
    CIMap.add n (nentry, nexit) m) cmap (range (max_lab - 1))
  in 
    if (CIMap.equal ci_converged cmap ncmap) then ncmap
    else f ncmap;;

let perform dfg max_lab =
  let sp = start_point max_lab in
  let fset = final_set dfg max_lab in
  fix (lv_iterate dfg fset max_lab) sp;;
