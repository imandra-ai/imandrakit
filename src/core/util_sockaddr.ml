(** Socket addresses *)

open Imandrakit_error

type t = Unix.sockaddr

let show : t -> string = function
  | Unix.ADDR_INET (host, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr host) port
  | Unix.ADDR_UNIX s -> spf "unix://%s" s

let pp = Fmt.of_to_string show

(** Error during DNS resolution *)
let resolve_error : Imandrakit_error.Kind.t =
  Imandrakit_error.Kind.make ~name:"SockaddrParseError" ()

let parse_or_resolve_inet_addr (s : string) ~port : t =
  match Unix.inet_addr_of_string s with
  | addr -> Unix.ADDR_INET (addr, port)
  | exception _ ->
    (match Unix.getaddrinfo s (string_of_int port) [] with
    | [] -> Error.failf ~kind:resolve_error "Cannot resolve address for %S." s
    | a :: _ -> a.Unix.ai_addr)

let kind = function
  | Unix.ADDR_INET (a, _) ->
    if Unix.is_inet6_addr a then
      Unix.PF_INET6
    else
      Unix.PF_INET
  | Unix.ADDR_UNIX _ -> Unix.PF_UNIX

(** Error in address parsing *)
let parse_error : Imandrakit_error.Kind.t =
  Imandrakit_error.Kind.make ~name:"SockaddrParseError" ()

(** Parse address *)
let parse : string -> t Error.result =
 fun s ->
  match CCString.chop_prefix ~pre:"unix://" s with
  | Some s -> Ok (Unix.ADDR_UNIX s)
  | None ->
    (try
       match CCString.Split.left ~by:":" s with
       | Some (lhs, rhs) ->
         let rhs =
           try int_of_string rhs
           with _ -> Error.failf ~kind:parse_error "Invalid port: %S." rhs
         in
         Ok (parse_or_resolve_inet_addr lhs ~port:rhs)
       | None -> Ok (parse_or_resolve_inet_addr s ~port:0)
     with exn ->
       let bt = Printexc.get_raw_backtrace () in
       let err = Error.of_exn ~bt ~kind:parse_error exn in
       Error err)

let parse_exn : string -> t = fun s -> parse s |> Error.unwrap
