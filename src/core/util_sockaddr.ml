(** Socket addresses *)

type t = Unix.sockaddr

let show : t -> string = function
  | Unix.ADDR_INET (host, port) ->
    spf "%s:%d" (Unix.string_of_inet_addr host) port
  | Unix.ADDR_UNIX s -> spf "unix://%s" s

let pp = Fmt.of_to_string show

let parse_or_resolve_inet_addr (s : string) ~port : t =
  match Unix.inet_addr_of_string s with
  | addr -> Unix.ADDR_INET (addr, port)
  | exception _ ->
    (match Unix.getaddrinfo s (string_of_int port) [] with
    | [] -> failwith @@ spf "Cannot resolve address for %S." s
    | a :: _ -> a.Unix.ai_addr)

(** Parse address *)
let parse : string -> (t, [ `msg of string ]) result =
 fun s ->
  match CCString.chop_prefix ~pre:"unix://" s with
  | Some s -> Ok (Unix.ADDR_UNIX s)
  | None ->
    (try
       match CCString.Split.left ~by:":" s with
       | Some (lhs, rhs) ->
         let rhs =
           try int_of_string rhs
           with _ -> failwith @@ spf "Invalid port: %S." rhs
         in
         Ok (parse_or_resolve_inet_addr lhs ~port:rhs)
       | None -> Ok (parse_or_resolve_inet_addr s ~port:0)
     with exn -> Error (`msg (Printexc.to_string exn)))

let parse_exn : string -> t =
 fun s ->
  match parse s with
  | Ok x -> x
  | Error (`msg s) -> failwith s
