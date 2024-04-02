type t = { b: bytes } [@@unboxed]

let create n =
  let len = (n lsr 3) + 1 in
  { b = Bytes.make len '\x00' }

let to_bytes self = self.b
let of_bytes b : t = { b }
let[@inline] length self = Bytes.length self.b lsl 3

let[@inline] get (self : t) i =
  let c = Bytes.get self.b (i lsr 3) |> Char.code in
  let mask = 1 lsl (i land 0b111) in
  c land mask != 0

let[@inline] set (self : t) i : unit =
  let idx = i lsr 3 in
  let c = Bytes.get self.b idx |> Char.code in
  let mask = 1 lsl (i land 0b111) in
  Bytes.unsafe_set self.b idx (Char.unsafe_chr (c lor mask))

let of_list l : t =
  assert (List.for_all (fun x -> x >= 0) l);
  let max = List.fold_left max 1 l in
  let self = create max in
  List.iter (set self) l;
  self

let all_ones n =
  let self = create n in
  for i = 0 to n - 1 do
    set self i
  done;
  self

let iter (self : t) f : unit =
  for idx = 0 to Bytes.length self.b - 1 do
    let c = Bytes.unsafe_get self.b idx |> Char.code in
    for j = 0 to 7 do
      if c land (1 lsl j) != 0 then (
        let i = (idx lsl 3) + j in
        f i
      )
    done
  done

let to_list self : _ list =
  let l = ref [] in
  iter self (fun x -> l := x :: !l);
  !l
