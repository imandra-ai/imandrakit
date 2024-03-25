type message = {
  msg: string;
  data: Data.t;
  bt: string option; [@key "bt"]
}

type stack = message list

type t = {
  process: string; [@key "p"]
  kind: Kind.t;
  msg: message; [@key "msg"]
  stack: stack;
}

let encode_message (self : message) =
  Value.(
    let d = Str_map.singleton "msg" (string self.msg) in
    let d =
      if Data.is_empty self.data then
        d
      else
        Str_map.add "data" (Codec.encode Data.codec self.data) d
    in
    let d =
      match self.bt with
      | None -> d
      | Some bt -> Str_map.add "bt" (string bt) d
    in
    dict d)

exception E of t

let raise_err ?bt (e : t) =
  Logs.debug (fun k -> k "(@[util.error.raise %s@])" e.msg.msg);
  match bt with
  | None -> raise (E e)
  | Some bt -> Printexc.raise_with_backtrace (E e) bt

module Internal_ = struct
  let process_name_ = ref "/1proc"
  let get_process_name () = !process_name_
end

module Message = struct
  type t = message

  let data (self : t) = self.data
  let get k (self : t) = Data.get k self.data

  let pp out (self : t) : unit =
    let pp_bt out () =
      match self.bt with
      | None | Some "" -> ()
      | Some bt -> Fmt.fprintf out "@ backtrace:@ %a" Fmt.string_lines bt
    in
    Fmt.fprintf out "@[<v>%a%a@]" Fmt.string_lines self.msg pp_bt ()

  let show = Fmt.to_string pp
end

let data (self : t) = self.msg.data
let get_data k (self : t) = Data.get k self.msg.data

let add_bt bt (self : t) : t =
  let bt =
    match self.msg.bt with
    | None -> Some bt
    | Some bt' -> Some (bt ^ "\nin:\n" ^ bt')
    (* merge *)
  in
  { self with msg = { self.msg with bt } }

let add_ctx msg (self : t) : t = { self with stack = msg :: self.stack }

let add_data k v (self : t) : t =
  let msg = { self.msg with data = Data.add k v self.msg.data } in
  { self with msg }

type !'a result = ('a, t) Stdlib.result

let map_result = CCResult.map
let iter_result = CCResult.iter

let unwrap = function
  | Ok x -> x
  | Error e -> raise_err e

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
        process = Internal_.get_process_name ();
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
