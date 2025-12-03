type message = {
  msg: string;
  bt: string option; [@key "bt"]
}
[@@deriving twine, typereg]

type stack = message list [@@deriving twine, typereg]

type +'a t = {
  err: 'a; [@key "e"]
  process: string; [@key "p"]
  data: Data.t; [@key "dt"]
  stack: stack; [@key "st"]
}
[@@deriving show { with_path = false }, twine, typereg]

type ('a, 'err) or_error = ('a, 'err t) Imandrakit_twine.Util.Result.t
[@@deriving show, eq, twine, typereg]

type 'err ectx = {
  ectx_raise: 'b. Printexc.raw_backtrace option -> 'err t -> 'b;
}
[@@unboxed]

let with_ectx (type err) (f : err ectx -> 'b) : ('b, err) or_error =
  let exception E of err t in
  let ectx =
    {
      ectx_raise =
        (fun bt e ->
          match bt with
          | None -> raise_notrace (E e)
          | Some bt -> Printexc.raise_with_backtrace (E e) bt);
    }
  in
  try
    let x = f ectx in
    Ok x
  with E e -> Error e

let[@inline] raise_err ?bt ectx (e : _ t) = ectx.ectx_raise bt e

module Message = struct
  type t = message

  let pp out (self : t) : unit =
    let pp_bt out () =
      match self.bt with
      | Some "" -> ()
      | Some bt when Printexc.backtrace_status () ->
        Fmt.fprintf out "@ backtrace:@ %a" Fmt.string_lines bt
      | _ -> ()
    and pp_data out d =
      Data.iter d (fun (Data.B (k, v)) -> Data.Key.pp k out v)
    in
    if Data.is_empty self.data then
      Fmt.fprintf out "@[<v>%a%a@]" Fmt.string_lines self.msg pp_bt ()
    else
      Fmt.fprintf out "@[<v>%a@,%a%a@]" Fmt.string_lines self.msg pp_data
        self.data pp_bt ()

  let show = Fmt.to_string pp
end

let data (self : _ t) = self.data
let get k (self : _ t) = Data.get k self.data

let add_bt bt (self : 'err t) : 'err t =
  let bt =
    match self.bt with
    | None -> Some bt
    | Some bt' -> Some (bt ^ "\nin:\n" ^ bt')
    (* merge *)
  in
  { self with bt }

let add_ctx msg (self : 'e t) : 'e t = { self with stack = msg :: self.stack }

let add_data k v (self : 'e t) : 'e t =
  { self with data = Data.add k v self.data }

let map_result = CCResult.map
let iter_result = CCResult.iter

let[@inline] unwrap ectx = function
  | Ok x -> x
  | Error e -> raise_err ectx e

let guard ?(let_pass = fun _ -> false) g f =
  try f () with
  | E e ->
    let bt = Printexc.get_raw_backtrace () in
    let msg = { (g ()) with bt = Some (Printexc.raw_backtrace_to_string bt) } in
    let e = add_ctx msg e in
    raise_err ~bt e
  | exn when let_pass exn ->
    (* pass-through *)
    let bt = Printexc.get_raw_backtrace () in
    Printexc.raise_with_backtrace exn bt
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    let e =
      {
        kind = Kind.generic_internal_error;
        process = Global_process_data.get_process_name ();
        stack = [];
        msg =
          {
            msg = Printexc.to_string exn;
            data = Data.empty;
            bt = Some (Printexc.raw_backtrace_to_string bt);
          };
      }
    in
    let msg = g () in
    let e = add_ctx msg e in
    raise_err ~bt e

module Infix = struct
  let ( let*! ) x f = guard x f
end

include Infix

let hr_ = String.make 76 '-'

let rec pp_stack out = function
  | [] -> ()
  | msg :: l ->
    (* print deeper levels first *)
    pp_stack out l;
    Fmt.fprintf out "@ %s@,@[<hv2>@{<Blue>Context@}:@ %a@]" hr_ Message.pp msg

let pp_with ~show_process out (self : t) : unit =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "error.pp" in
  let meta =
    if show_process then
      spf "[%s]%s" self.process (Kind.show self.kind)
    else
      Kind.show self.kind
  in
  Fmt.fprintf out "@[<v>@[<v2>@{<Red>Error@}%s:@ %a@]%a@]" meta Message.pp
    self.msg pp_stack self.stack

let pp out self = pp_with ~show_process:false out self

let show self =
  let buf = Buffer.create 16 in
  let out = Format.formatter_of_buffer buf in
  Fmt.set_color_tag_handling out;
  pp out self;
  Format.pp_print_flush out ();
  Buffer.contents buf

let () =
  Printexc.register_printer (function
    | E err -> Some (show err)
    | _ -> None)

let pp_result ppx out (x : _ result) = Fmt.Dump.result' ppx pp out x
let show_result ppx = Fmt.to_string (pp_result ppx)
