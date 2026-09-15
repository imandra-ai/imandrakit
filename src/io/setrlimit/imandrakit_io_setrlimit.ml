type resource =
  | RLIMIT_CORE  (** maximum size of a core file, in bytes *)
  | RLIMIT_CPU  (** maximum amount of CPU time, in seconds, used by a process *)
  | RLIMIT_DATA  (** maximum size of a data segment of the process, in bytes *)
  | RLIMIT_FSIZE
      (** maximum size of a file, in bytes, that may be created by a process *)
  | RLIMIT_NOFILE
      (** a number one greater than the maximum value that the system may assign
          to a newly-created descriptor *)
  | RLIMIT_STACK
      (** maximum size of the initial thread's stack, in bytes. This might clash
          with OCaml's stack management. *)
  | RLIMIT_AS
      (** maximum size of total available memory of the process, in bytes. This
          counts reserved address space rather than memory in use; see
          {!set_memory_limit_exn}. *)
[@@deriving show { with_path = false }]

module Raw = struct
  let resource_to_int = function
    | RLIMIT_CORE -> 0n
    | RLIMIT_CPU -> 1n
    | RLIMIT_DATA -> 2n
    | RLIMIT_FSIZE -> 3n
    | RLIMIT_NOFILE -> 4n
    | RLIMIT_STACK -> 5n
    | RLIMIT_AS -> 6n

  external get : nativeint -> (nativeint * nativeint, nativeint) Result.t
    = "caml_imandrakit_getrlimit"

  external set : nativeint -> nativeint -> nativeint -> int
    = "caml_imandrakit_setrlimit"
  (** [0] on success, a positive errno if [setrlimit] failed, or a negative
      errno if the preliminary [getrlimit] failed. *)

  external address_space : unit -> nativeint = "caml_imandrakit_address_space"
  external strerror : nativeint -> string = "caml_imandrakit_rlimit_strerror"
end

type limits = {
  cur: nativeint option; (* Soft limit; [None] means unlimited *)
  max: nativeint option; (* Hard limit; [None] means unlimited *)
}
[@@deriving show { with_path = false }]

type error = {
  call: string;  (** the syscall that failed, ["setrlimit"] or ["getrlimit"] *)
  errno: nativeint;
  msg: string;  (** ready-made human readable message *)
}
[@@deriving show { with_path = false }]

let einval = 22n

(** Address space currently reserved by this process, in bytes, if the platform
    can report it. This is what {!RLIMIT_AS} is measured against, and it counts
    reservations that hold no memory: a macOS process has hundreds of GiB of
    them before [main] runs. *)
let current_address_space () : nativeint option =
  let v = Raw.address_space () in
  if v = Nativeint.minus_one then
    None
  else
    Some v

let show_limit = function
  | None -> "unlimited"
  | Some n -> Nativeint.to_string n

(* Only claims what it can check: that the request was below the address space
   already reserved, which is what macOS rejects with EINVAL. *)
let hint (r : resource) (l : limits) (errno : nativeint) : string =
  match r, l.max, current_address_space () with
  | (RLIMIT_AS | RLIMIT_DATA), Some want, Some used
    when errno = einval && want < used ->
    Printf.sprintf
      " (the process already has %nd bytes of address space reserved, and %s \
       counts reserved address space rather than memory in use; use \
       set_memory_limit_exn to express the limit relative to it)"
      used (show_resource r)
  | _ -> ""

let explain (r : resource) (l : limits) (call : string) (errno : nativeint) :
    string =
  Printf.sprintf "%s(%s, cur=%s, max=%s) failed: %s%s" call (show_resource r)
    (show_limit l.cur) (show_limit l.max) (Raw.strerror errno) (hint r l errno)

(** [set_result resource limits] lowers the limits for [resource]. A limit is
    only ever lowered, never raised; [None] means "unlimited", which in practice
    leaves the corresponding limit untouched. *)
let set_result (r : resource) (l : limits) : (unit, error) Result.t =
  let cur : nativeint = Option.value l.cur ~default:Nativeint.minus_one in
  let max : nativeint = Option.value l.max ~default:Nativeint.minus_one in
  match Raw.set (Raw.resource_to_int r) cur max with
  | 0 -> Ok ()
  | code ->
    let call, errno =
      if code < 0 then
        "getrlimit", Nativeint.of_int (-code)
      else
        "setrlimit", Nativeint.of_int code
    in
    Error { call; errno; msg = explain r l call errno }

(** [set resource cur max] returns [true] if setting the limit succeeded. See
    {!set_result} for the reason when it does not. *)
let set (r : resource) (cur : nativeint) (max : nativeint) : bool =
  Result.is_ok (set_result r { cur = Some cur; max = Some max })

(** Like {!set_result}, but propagates failures
    @raise Failure if it fails *)
let set_exn (r : resource) (l : limits) : unit =
  match set_result r l with
  | Ok () -> ()
  | Error { msg; _ } -> failwith msg

let set_hard_exn (r : resource) (max : nativeint option) : unit =
  set_exn r { cur = None; max }

(** [set_memory_limit_exn ~bytes] limits the process to [bytes] of additional
    memory.

    {!RLIMIT_AS} counts reserved address space, not memory in use, and how much
    a process reserves before it starts differs wildly by platform: a few MiB on
    Linux, but hundreds of GiB on macOS, which also refuses any limit below what
    is already reserved. An absolute byte count is therefore rejected on macOS
    for every value small enough to be a useful cap. This expresses the limit
    relative to the address space already reserved instead.

    The limit is on growth from this call, not on total footprint, so set it
    early in startup. Memory in use can never exceed reserved address space, so
    the bound is conservative and may trip before [bytes] are really used.

    @raise Failure if the limit cannot be set *)
let set_memory_limit_exn ~(bytes : nativeint) : unit =
  if bytes <= 0n then invalid_arg "set_memory_limit_exn: bytes must be positive";
  let max =
    match current_address_space () with
    | None -> bytes
    | Some used ->
      let total = Nativeint.add used bytes in
      if total < used then
        (* overflowed: ask for everything that is representable *)
        Nativeint.max_int
      else
        total
  in
  set_exn RLIMIT_AS { cur = None; max = Some max }

let get (r : resource) : (limits, nativeint) Result.t =
  match Raw.get (Raw.resource_to_int r) with
  | Ok (cur, max) ->
    Ok
      {
        cur =
          (if cur = Nativeint.minus_one then
             None
           else
             Some cur);
        max =
          (if max = Nativeint.minus_one then
             None
           else
             Some max);
      }
  | Error e -> Error e
