open While_ast;;
open Data_flow;;
open Lv_analysis;;
open Utils;;

let eliminate_dead_assignments ast =
  let (dfg, max_lab) = build_data_flow_graph ast in
  let lv_result = Lv_analysis.perform dfg max_lab in
  let can_eliminate asgn =
    let eblock_ind = List.find (fun n ->
      let eblock = EBSet.find n dfg in
        match eblock.content with
        | Left s -> s = asgn
        | Right _ -> false) (range (max_lab-1)) in
    let (_, exit) = CIMap.find eblock_ind lv_result in
      IdentSet.is_empty exit in
  let rec eliminate ast =
    match ast with
    | IfStmt (cond, truestmt, falsestmt, n) ->
        let truestmtelim = eliminate truestmt and
            falsestmtelim = eliminate falsestmt in
        IfStmt (cond, truestmtelim, falsestmtelim, n)
    | WhileStmt (cond, body, n) ->
        let bodyelim = eliminate body in
        WhileStmt (cond, bodyelim, n)
    | CompStmt (stmt1, stmt2, n) ->
        let stmt1elim = eliminate stmt1 and
            stmt2elim = eliminate stmt2 in
        CompStmt (stmt1elim, stmt2elim, n)
    | SkipStmt n ->
        SkipStmt n
    | AssignStmt (_, _, _) as asgn ->
        if can_eliminate asgn then
          DeadExpr
        else
          asgn
    | x -> x
  in eliminate ast;;
