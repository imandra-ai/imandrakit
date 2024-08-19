type resource =
  | RLIMIT_CORE  (** maximum size of a core file, in bytes *)
  | RLIMIT_CPU  (** maximum amount of CPU time, in seconds, used by a process *)
  | RLIMIT_DATA  (** maximum size of a data segment of the process, in bytes *)
  | RLIMIT_FSIZE
      (** maximum size of a file, in bytes, that may be created by a process *)
  | RLIMIT_NOFILE
      (** a number one greater than the maximum value that the system may assign to a newly-created descriptor *)
  | RLIMIT_STACK
      (** maximum  size of the initial thread's stack, in bytes. This might clash with OCaml's stack management. *)
  | RLIMIT_AS
      (**  maximum size of total available memory of the process, in bytes *)

module Raw = struct
  let resource_to_int = function
    | RLIMIT_CORE -> 0
    | RLIMIT_CPU -> 1
    | RLIMIT_DATA -> 2
    | RLIMIT_FSIZE -> 3
    | RLIMIT_NOFILE -> 4
    | RLIMIT_STACK -> 5
    | RLIMIT_AS -> 6

  external set : int -> int -> bool = "caml_imandrakit_setrlimit"
end

(** [set resource limit] returns [true] if setting the limit succeeded *)
let[@inline] set (r : resource) (v : int) : bool =
  Raw.set (Raw.resource_to_int r) v

(** Like {!set}, but propagates failures
  @raise Failure if it fails *)
let set_exn r v : unit = if not (set r v) then failwith "setrlimit failed"
