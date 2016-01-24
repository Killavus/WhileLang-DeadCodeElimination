open Data_flow;;
open While_ast;;

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
    | BIdent x -> IdentSet.add x s
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
     | AssignStmt (_, aexpr) -> free_variables_aexpr aexpr
     | _ -> IdentSet.empty
    );;

let killc c =
  match c with
  | Right _ -> IdentSet.empty
  | Left stmt ->
      (match stmt with
       | AssignStmt (x, _) -> IdentSet.singleton x
       | _ -> IdentSet.empty);;

let gen dfn =
  match dfn with
  | { content = c ; children = _ } -> genc c;;

let kill dfn =
  match dfn with
  | { content = c ; children = _ } -> killc c;;

let range n =
  let rec range_ n acc =
    if n = -1 then
      acc
    else 
      range_ (n-1) (n::acc)
  in range_ n [];;

let copy_dfg_without_links dfg max_lab =
  let inversed_dfg = EBSet.empty in
  List.fold_left (fun ndfg lab ->
    let node = EBSet.find lab dfg in
    EBSet.add lab { node with children = [] } ndfg
  ) inversed_dfg (range (max_lab - 1));; 

let inverse_data_flow_graph dfg max_lab = 
  let dfgmod = copy_dfg_without_links dfg max_lab in
    List.fold_left (fun ng lab ->
      let node = EBSet.find lab dfg in
      List.fold_left (fun ng_ clab ->
        let connected_node = EBSet.find clab ng in
        EBSet.add clab { connected_node with 
          children = (lab::connected_node.children) } ng_) ng node.children
    ) dfgmod (range (max_lab - 1));;

let final_set dfg max_lab = 
  List.fold_left (fun finalset lab ->
    let node = EBSet.find lab dfg in
      match List.filter (fun chl -> chl > lab) node.children with
      | [] -> LabelSet.add lab finalset
      | _ -> finalset) LabelSet.empty (range (max_lab - 1));;