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
      (** maximum size of total available memory of the process, in bytes *)

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

  external set : nativeint -> nativeint -> nativeint -> bool
    = "caml_imandrakit_setrlimit"
end

(** [set resource limit] returns [true] if setting the limit succeeded *)
let[@inline] set (r : resource) (cur : nativeint) (max : nativeint) : bool =
  Raw.set (Raw.resource_to_int r) cur max

type limits = {
  cur: nativeint option; (* Soft limit *)
  max: nativeint option; (* Hard limit *)
}
[@@deriving show { with_path = false }]

(** Like {!set}, but propagates failures
    @raise Failure if it fails *)
let set_exn (r : resource) (l : limits) : unit =
  let cur : nativeint = Option.value l.cur ~default:Nativeint.minus_one in
  let max : nativeint = Option.value l.max ~default:Nativeint.minus_one in
  if not (set r cur max) then failwith "setrlimit failed"

let set_hard_exn (r : resource) (max : nativeint option) : unit =
  set_exn r { cur = None; max }

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
