open While_ast;;

type ('a, 'b) either = Left of 'a | Right of 'b;;

(* Elementary Blocks Set - effectively a graph representation *)
module EBSet = Map.Make(struct type t = int let compare = compare end);;

(* Data Flow Node. *)
type data_flow_tree = {
  content : (stmt, bool_expr) either;
  children: EBSet.key list;
};;

let link_df ebs links cur =
  List.fold_left (
    fun ebs_ link ->
      let dfn = EBSet.find link ebs_ in
        EBSet.add link { dfn with children = (cur::dfn.children) } ebs_
  ) ebs links;;

let rec build_df ast ebs links next_cur =
  match ast with
  | SkipStmt -> (ebs, links, next_cur)
  | CompStmt (s1, s2) ->
    let (s1ebs, s1links, s1ncur) = build_df s1 ebs links next_cur in
    build_df s2 s1ebs s1links s1ncur 
  | WhileStmt (b, ws) ->
      let ebsm = link_df ebs links next_cur in
      let bool_node = { content = Right b; children = [] } in
      let ebsmb = EBSet.add next_cur bool_node ebsm in 
      let (ebsfb, wlinks, wncur) = build_df ws ebsmb [next_cur] (next_cur + 1) in
        if wncur > (next_cur + 1) then
          let last_node = EBSet.find (wncur - 1) ebsfb in
          let tied_knot_ebsfb = 
          EBSet.add (wncur - 1) { last_node with 
            children = (last_node.children @ [next_cur]) } ebsfb in
          (tied_knot_ebsfb, [next_cur], wncur)
        else
          (ebsfb, [next_cur], next_cur + 1)
  | IfStmt (b, ts, fs) ->
      let new_node = { content = Right b; children = [] } in
      let ebsb = EBSet.add next_cur new_node ebs in
      let ebsmb = link_df ebsb links next_cur in
      let (tsebsmb, tslinks, tscur) = 
        build_df ts ebsmb [next_cur] (next_cur + 1) in
      let (fsebsmb, fslinks, fscur) = build_df fs tsebsmb [next_cur] tscur in
        if tscur > (next_cur + 1) then
          if fscur > tscur then
            (* Assignments on both paths *)
            (fsebsmb, [tscur - 1; fscur - 1], fscur)
          else
            (* Assignments only on true path *)
            (fsebsmb, [next_cur; tscur - 1], fscur)
        else
          if fscur > tscur then
            (* Assignments only on false path *)
            (fsebsmb, [next_cur; fscur - 1], fscur)
          else
            (* No assignments on both true/false path. *)
            (fsebsmb, [next_cur], fscur)
  | asgn ->
      let ebsm = link_df ebs links next_cur in
      (EBSet.add next_cur { content = Left asgn; children = [] } ebsm, [next_cur], next_cur + 1);;

let build_data_flow_tree ast = build_df ast (EBSet.empty) [] 0;;

