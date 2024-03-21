let ptime_now () = Ptime_clock.now () |> Ptime.to_float_s
let mtime_now () = Mtime_clock.now ()

let[@inline] time_since_start () =
  let t = Mtime_clock.elapsed () in
  Mtime.Span.to_float_ns t /. 1e9

let mtime_now_s = time_since_start

module Stopwatch = struct
  type t = {
    start: Mtime.t;
    mutable stop: Mtime.t option;
  }

  let stop (w : t) = w.stop <- Some (mtime_now ())

  (** Time elapsed since it started *)
  let time (w : t) : float =
    let span =
      match w.stop with
      | None -> Mtime.span (mtime_now ()) w.start
      | Some t -> Mtime.span t w.start
    in
    Mtime.Span.to_float_ns span /. 1e9

  let create () : t =
    let now = mtime_now () in
    { start = now; stop = None }

  let timeit f =
    let t = create () in
    let x = f () in
    time t, x
end

let remove_dups_with (type k) (module Tbl : CCHashtbl.S with type key = k) lst =
  let seen = Tbl.create (List.length lst) in
  List.filter
    (fun x ->
      let tmp = not (Tbl.mem seen x) in
      Tbl.replace seen x ();
      tmp)
    lst

let pp_text_newlines = Fmt.string_lines
let pp_quoted out s = Fmt.fprintf out "%S" s
let pp_atomic ppx out a = ppx out (Atomic.get a)

let pp_list ?(sep = "") ppx out l =
  Fmt.(list ~sep:(fun out () -> Fmt.fprintf out "%s@ " sep) ppx) out l

let pp_iter ?(sep = "") ppx out it =
  Fmt.(iter ~sep:(fun out () -> Fmt.fprintf out "%s@ " sep) ppx) out it

let pp_backquote pp out x = Fmt.fprintf out "`%a`" pp x

let parse_tcp_addr addr =
  try
    let addr, port =
      match CCString.Split.right ~by:":" addr with
      | None -> addr, 1234
      | Some (x, y) -> x, int_of_string y
    in
    let addr =
      if addr = "localhost" then
        Unix.inet_addr_loopback
      else
        Unix.inet_addr_of_string addr
    in
    Ok (addr, port)
  with e ->
    Error
      (Printf.sprintf "cannot parse address %S: %s" addr (Printexc.to_string e))

let uuid_v4 () : string =
  let uuid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
  Uuidm.to_string uuid

let this_process_uuid = uuid_v4 ()

let true_in_env s =
  match Sys.getenv_opt s with
  | Some "true" | Some "1" -> true
  | _ -> false

let format_datetime (time : float) : string =
  let tm = Unix.localtime time in
  let msec = (time -. floor time) *. 1_000. |> int_of_float in
  Printf.sprintf "%02d-%02d-%02dT%02d:%02d:%02d.%03d" (tm.tm_year - 100)
    tm.tm_mday (tm.tm_mon + 1) tm.tm_hour tm.tm_min tm.tm_sec msec

let pp_datetime = Fmt.of_to_string format_datetime

let format_duration_s (f : float) : string =
  let nb_sec_minute = 60 in
  let nb_sec_hour = 60 * nb_sec_minute in
  let nb_sec_day = 24 * nb_sec_hour in
  let n = int_of_float f in
  if n >= 1 then (
    let aux n div = n / div, n mod div in
    let n_day, n = aux n nb_sec_day in
    let n_hour, n = aux n nb_sec_hour in
    let n_min, n = aux n nb_sec_minute in
    let print_aux s n =
      if n <> 0 then
        string_of_int n ^ s
      else
        ""
    in
    print_aux "d" n_day ^ print_aux "h" n_hour ^ print_aux "m" n_min
    ^ string_of_int n
    ^ (if f -. floor f >= 0.01 then (
        let s = Printf.sprintf "%.1f" (f -. floor f) in
        (* remove the leading "0." *)
        "." ^ snd @@ CCString.Split.left_exn ~by:"." s
      ) else
        "")
    ^ "s"
  ) else if f < 0.010 then
    spf "%.2fms" (f *. 1000.)
  else
    spf "%.1fms" (f *. 1000.)

let pp_duration_s = Fmt.of_to_string format_duration_s

let format_byte_size (x : int) : string =
  let xf = float x in
  if xf >= 1e9 then
    spf "%.3fGB" (xf /. 1e9)
  else if xf >= 1e6 then
    spf "%.3fMB" (xf /. 1e6)
  else if xf >= 1e3 then
    spf "%.3fkB" (xf /. 1e3)
  else
    spf "%dB" x

let pp_byte_size out s = Fmt.string out (format_byte_size s)

let str_contains s1 s2 =
  let len2 = String.length s2 in
  let found = ref false in
  let continue = ref true in
  let i = ref 0 in

  while !continue && !i <= String.length s1 - len2 do
    let j = ref 0 in
    let fail = ref false in
    while (not !fail) && !j < len2 do
      if String.get s1 (!i + !j) <> String.get s2 !j then fail := true;
      incr j
    done;
    if not !fail then (
      continue := false;
      found := true
    ) else
      incr i
  done;
  !found

let str_limit_len max_len s : string =
  if String.length s <= max_len then
    s
  else
    spf "%s[…(%dB omitted)]" (String.sub s 0 max_len) (String.length s - max_len)

let reset_line_ansi = "\x1b[2K\r"

module List = struct
  let rec split3 l =
    match l with
    | [] -> [], [], []
    | (x, y, z) :: tl ->
      let l1, l2, l3 = split3 tl in
      x :: l1, y :: l2, z :: l3
end

module Either = CCEither

type ('a, 'b) either = ('a, 'b) CCEither.t =
  | Left of 'a
  | Right of 'b

let pp_either pp1 pp2 out = function
  | Left x -> Format.fprintf out "(@[Left@ %a@])" pp1 x
  | Right x -> Format.fprintf out "(@[Right@ %a@])" pp2 x

let min_opt_int (a : int option) (b : int option) : int option =
  match a, b with
  | Some x, Some y -> Some (min x y)
  | Some x, None -> Some x
  | None, Some y -> Some y
  | _ -> None
