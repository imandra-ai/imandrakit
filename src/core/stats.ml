(** Statistics output.

    This provides a set of statistics that can be created after a solver is
    done, to print them all. Solvers can keep internal counters and add them to
    {!Stats.t} when asked to. *)

type t = { mutable stats: int Util_twine.Str_map.t } [@@deriving twine]

let pp out (self : t) : unit =
  Fmt.fprintf out "{ @[";
  Str_map.iter (fun name c -> Fmt.fprintf out "%s = %d;@ " name c) self.stats;
  Fmt.fprintf out "@]}"

let create () : t = { stats = Str_map.empty }
let add (self : t) name i : unit = self.stats <- Str_map.add name i self.stats

(** Iterate on all stats *)
let iter (self : t) : (string * int) Iter.t =
 fun k -> Str_map.iter (fun s i -> k (s, i)) self.stats
