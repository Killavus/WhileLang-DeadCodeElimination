open While_ast;;

type ('a, 'b) either = Left of 'a | Right of 'b;;

(* Elementary Blocks Set - effectively a graph representation *)
module EBSet = Map.Make(struct type t = int let compare = compare end);;

type data_flow_tree = {
  content : (stmt, bool_expr) either;
  children: data_flow_tree list;
};;

let link_df ebs links cur =
  List.fold_left (
    fun ebs_ link ->
      let dfn = EBSet.find link ebs_ in
        EBSet.add link { dfn with children = (cur::dfn.children) } ebs_
  ) ebs links;;

let build_while b ws ebs links next_cur =
  let ebsm = link_df ebs links next_cur in
    let (ebsf, wlinks, wncur) = build_df ws esbm [next_cur] (next_cur + 1) in
      if wncur > (next_cur + 1) then
        let last_node = EBSet.find (wncur - 1) esbf in
        let tied_knot_ebsf = 
          EBSet.add (wncur - 1) { last_node with children = (last_node.children @ [cur]) } in
        let bool_node = { content = Right b; children = [next_cur + 1] } in
        (EBSet.add next_cur bool_node tied_knot_ebsf, [next_cur], wncur)
      else
        (EBSet.add next_cur bool_node ebsf, [next_cur], next_cur + 1);;

let build_if b ts fs ebs links next_cur =
  let ebsm = link_df ebs links next_cur in
  let (tsebsm, tslinks, tscur) = build_df ts esbm [next_cur] (next_cur + 1) in
  let (fsebsm, fslinks, fscur) = build_df fs tsebsm [next_cur] tscur in
    if tscur > (next_cur + 1) then
      if fscur > tscur then
        let new_node = { content = Right b; children = [next_cur + 1; tscur] } in
        (EBSet.add next_cur new_node fsebsm, [tscur - 1; fscur - 1], fscur)
      else
        let new_node = { content = Right b; children = [next_cur + 1] } in
        (EBSet.add next_cur new_node fsebsm, [next_cur; tscur - 1], fscur)
    else
      if fscur > tscur then
        let new_node = { content = Right b; children = [next_cur + 1] } in
        (EBSet.add next_cur new_node fsebsm, [next_cur; fscur - 1], fscur)
      else
        let new_node = { content = Right b; children = [] } in
        (EBSet.add next_cur new_node fsebsm, [next_cur], fscur);; 

let rec build_df ast ebs links next_cur =
  match ast with
  | SkipStmt -> (ebs, links, next_cur)
  | CompStmt (s1, s2) ->
    let (s1ebs, s1links, s1ncur) = build_df s1 ebs links next_cur in
    build_df s2 s1ebs s1links s1ncur 
  | WhileStmt (b, ws) ->
    build_while b ws ebs links next_cur
  | IfStmt (b, ts, fs) ->
    build_if b ts fs ebs links next_cur
  | asgn ->
      let ebsm = link_df ebs links cur in
      EBSet.add next_cur { content = Left asgn; children = [] } ebsm;;

let build_data_flow_tree ast = build_df ast EBSet.empty [] 0;;

