(** Statistics output.

    This provides a set of statistics that can be created after a solver is
    done, to print them all. Solvers can keep internal counters and add them to
    {!Stats.t} when asked to. *)

open struct
  module Ser_pack = Imandrakit_ser_pack

  let int_str_map_to_serpack =
    Ser_pack.Ser.(Util_serpack.Str_map.to_serpack (fun _ x -> int x))

  let int_str_map_of_serpack =
    Ser_pack.Deser.(Util_serpack.Str_map.of_serpack to_int)
end

type t = {
  mutable stats:
    (int Util_twine.Str_map.t
    [@ser int_str_map_to_serpack] [@deser int_str_map_of_serpack]);
}
[@@deriving serpack, twine]

let pp out (self : t) : unit =
  Fmt.fprintf out "{ @[";
  Str_map.iter (fun name c -> Fmt.fprintf out "%s = %d;@ " name c) self.stats;
  Fmt.fprintf out "@]}"

let create () : t = { stats = Str_map.empty }
let add (self : t) name i : unit = self.stats <- Str_map.add name i self.stats

(** Iterate on all stats *)
let iter (self : t) : (string * int) Iter.t =
 fun k -> Str_map.iter (fun s i -> k (s, i)) self.stats
