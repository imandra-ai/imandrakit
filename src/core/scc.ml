module type ARG = sig
  type t

  type node

  val children : t -> node -> node Iter.t

  module Node_tbl : Hashtbl.S with type key = node
end

module type S = sig
  module A : ARG

  val sccs : graph:A.t -> nodes:A.node list -> A.node list A.Node_tbl.t
end

module Make (A : ARG) = struct
  module A = A

  type state = {
    mutable min_id: int; (* min ID of the vertex' scc *)
    id: int; (* ID of the vertex *)
    mutable on_stack: bool;
    vertex: A.node;
  }

  let mk_cell v n = { min_id = n; id = n; on_stack = false; vertex = v }

  (* pop elements of [stack] until we reach node with given [id] *)
  let rec pop_down_to ~id acc stack =
    assert (not (Stack.is_empty stack));
    let cell = Stack.pop stack in
    cell.on_stack <- false;
    if cell.id = id then (
      assert (cell.id = cell.min_id);
      cell.vertex :: acc (* return SCC *)
    ) else
      pop_down_to ~id (cell.vertex :: acc) stack

  let sccs ~(graph : A.t) ~(nodes : A.node list) : A.node list A.Node_tbl.t =
    let res = A.Node_tbl.create 8 in
    let tbl = A.Node_tbl.create 16 in

    (* stack of nodes being explored, for the DFS *)
    let to_explore = Stack.create () in
    (* stack for Tarjan's algorithm itself *)
    let stack = Stack.create () in
    (* unique ID for new nodes *)
    let n = ref 0 in

    (* exploration starting from [v] *)
    let explore_from (v : A.node) : unit =
      Stack.push (`Enter v) to_explore;
      while not (Stack.is_empty to_explore) do
        match Stack.pop to_explore with
        | `Enter v ->
          if not (A.Node_tbl.mem tbl v) then (
            (* remember unique ID for [v] *)
            let id = !n in
            incr n;
            let cell = mk_cell v id in
            cell.on_stack <- true;
            A.Node_tbl.add tbl v cell;
            Stack.push cell stack;
            Stack.push (`Exit (v, cell)) to_explore;
            (* explore children *)
            let children = A.children graph v in
            Iter.iter (fun v' -> Stack.push (`Enter v') to_explore) children
          )
        | `Exit (v, cell) ->
          (* update [min_id] *)
          assert cell.on_stack;
          let children = A.children graph v in
          Iter.iter
            (fun dest ->
              (* must not fail, [dest] already explored *)
              let dest_cell = A.Node_tbl.find tbl dest in
              (* same SCC? yes if [dest] points to [cell.v] *)
              if dest_cell.on_stack then
                cell.min_id <- min cell.min_id dest_cell.min_id)
            children;
          (* pop from stack if SCC found *)
          if cell.id = cell.min_id then (
            let scc = pop_down_to ~id:cell.id [] stack in
            List.iter (fun node -> A.Node_tbl.add res node scc) scc
          )
      done
    in

    List.iter explore_from nodes;
    assert (Stack.is_empty stack);
    res
end
