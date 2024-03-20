(** Pretty printing in the DOT (graphviz) format.

    Example (print divisors from [42]):

    {[
      let open Dot in
      with_out "/tmp/truc.dot"
        (fun out ->
           pp ~attrs_v:(fun i -> [`Label (string_of_int i)]) ~graph:divisors_graph out 42
        )
    ]}

*)

type ('v, 'e) graph = 'v -> ('e * 'v) Iter.t
(** Directed graph with vertices of type ['v] and edges labeled with [e'] *)

type attribute =
  [ `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ]
(** Dot attribute *)

val pp :
  tbl:(module Hashtbl.S with type key = 'v) ->
  ?attrs_v:('v -> attribute list) ->
  ?attrs_e:('e -> attribute list) ->
  ?name:string ->
  graph:('v, 'e) graph ->
  'v Fmt.printer
(** Print the graph, starting from given vertex, on the formatter.
      @param attrs_v attributes for vertices.
      @param attrs_e attributes for edges.
      @param name name of the graph. *)

val pp_all :
  tbl:(module Hashtbl.S with type key = 'v) ->
  ?attrs_v:('v -> attribute list) ->
  ?attrs_e:('e -> attribute list) ->
  ?name:string ->
  graph:('v, 'e) graph ->
  'v Iter.t Fmt.printer
(** Same as {!pp} but starting from several vertices, not just one. *)

val with_out : string -> (Format.formatter -> 'a) -> 'a
(** Shortcut to open a file and write to it. *)

val escape_dot : string -> string
(** Escaping for graphviz labels *)

val default_style_dot : string
(** Default style for labels in graphviz *)
