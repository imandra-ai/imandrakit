type ('v, 'e) graph = 'v -> ('e * 'v) Iter.t

type attribute =
  [ `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ]
(** Dot attribute *)

let pp_list pp_x out l =
  Format.pp_print_string out "[";
  List.iteri
    (fun i x ->
      if i > 0 then Format.fprintf out ",@;";
      pp_x out x)
    l;
  Format.pp_print_string out "]"

type vertex_state = {
  mutable explored: bool;
  id: int;
}

(** Print an enum of Full.traverse_event *)
let pp_all (type node) ~(tbl : (module Hashtbl.S with type key = node))
    ?(attrs_v = fun _ -> []) ?(attrs_e = fun _ -> []) ?(name = "graph")
    ~(graph : _ graph) out iter =
  let (module Tbl) = tbl in
  let tbl = Tbl.create 32 in
  (* print an attribute *)
  let pp_attr out attr =
    match attr with
    | `Color c -> Format.fprintf out "color=%s" c
    | `Shape s -> Format.fprintf out "shape=%s" s
    | `Weight w -> Format.fprintf out "weight=%d" w
    | `Style s -> Format.fprintf out "style=%s" s
    | `Label l -> Format.fprintf out "label=\"%s\"" l
    | `Other (name, value) -> Format.fprintf out "%s=\"%s\"" name value
  (* map from vertices to integers *)
  and get_node =
    let count = ref 0 in
    fun v ->
      try Tbl.find tbl v
      with Not_found ->
        let node = { id = !count; explored = false } in
        incr count;
        Tbl.replace tbl v node;
        node
  and vertex_explored v =
    try (Tbl.find tbl v).explored with Not_found -> false
  in
  let set_explored v = (get_node v).explored <- true
  and get_id v = (get_node v).id in
  (* the unique name of a vertex *)
  let pp_vertex out v = Format.fprintf out "vertex_%d" (get_id v) in
  (* print preamble *)
  Format.fprintf out "@[<v2>digraph \"%s\" {@;" name;

  (* traverse *)
  let on_enter v =
    let attrs = attrs_v v in
    Format.fprintf out "@[<h>%a %a;@]@," pp_vertex v (pp_list pp_attr) attrs
  in
  let on_edge v1 e v2 : unit =
    let attrs = attrs_e e in
    Format.fprintf out "@[<h>%a -> %a %a;@]@," pp_vertex v1 pp_vertex v2
      (pp_list pp_attr) attrs
  in

  let rec traverse (n : node) : unit =
    if not (vertex_explored n) then (
      set_explored n;
      on_enter n;
      let edges = graph n in
      Iter.iter (fun (_, n2) -> traverse n2) edges;
      Iter.iter (fun (e, n2) -> on_edge n e n2) edges
    )
  in

  Iter.iter traverse iter;
  Format.fprintf out "}@]@;@?";
  ()

let pp ~tbl ?attrs_v ?attrs_e ?name ~graph fmt v =
  pp_all ~tbl ?attrs_v ?attrs_e ?name ~graph fmt (Iter.return v)

let with_out filename f =
  let@ oc = CCIO.with_out filename in
  let out = Format.formatter_of_out_channel oc in
  let x = f out in
  Fmt.flush out ();
  x

let default_style_dot = "fontname=\"courier\",fontsize=14"

let escape_dot s =
  let b = Buffer.create (String.length s + 5) in
  String.iter
    (fun c ->
      match c with
      | '|' | '\\' | '{' | '}' | '<' | '>' | '"' ->
        Buffer.add_char b '\\';
        Buffer.add_char b c
      | '\n' -> Buffer.add_string b "\\l"
      (* left justify *)
      | _ -> Buffer.add_char b c)
    s;
  Buffer.contents b
