(** Utils for IO *)

module Popen = Popen
module Xdg = Xdg

val str_of_file : string -> string
(** Read the content of a file *)

val write_to_file : string -> string -> unit
(** [write_to_file file s] writes [s] into [file] *)

val get_pid : unit -> int

type resolved_file = string
(** Resolved file (absolute path) *)

val make_absolute_in_memory : ?in_dir:string -> string -> string
(** Purely syntactic computation of absolute path. *)

val make_absolute : ?in_dir:string -> string -> resolved_file
(** Turn a file path into an absolute path, using {!Unix.realpath} *)

val find_file :
  ?in_dir:string ->
  file_name:string ->
  load_path:string list ->
  unit ->
  resolved_file option
(** Find a file.

    We look in this order:
    - [in_dir] or current directory if not provided,
    - load_path (left to right).

    Finally, we return a resolved_file.
*)

val with_signal : ?signal:int -> on_sig:(int -> unit) -> (unit -> 'a) -> 'a
(** Protection against signals when calling the given function *)

val block_sigpipe_sigint : unit -> unit
(** Block these signals in this thread *)

val read_i32_framed : in_channel -> string
(** [read_i32_framed ic] reads a int32_le-framed message from [ic] *)

val write_i32_framed : out_channel -> string -> unit
(** [write_i32_framed oc s] writes [s] as a int32_le-framed message onto [oc] *)

val with_capture_stdio_file :
  file:string ->
  ?redirect_stdout:bool ->
  ?redirect_stderr:bool ->
  append:bool ->
  unit ->
  (unit -> 'a) ->
  'a
(** [with_capture_stdio_file ~file ~append f] runs [f()] in a context
    where stdout/stderr are redirected to [file].
    This captures a lock because stdout/stderr are global resources.
    @param append specifies whether [file] is appended to, or truncated,
    if it exists already. *)

val with_capture_stdio_string :
  ?on_stdout:(bytes -> int -> int -> unit) ->
  ?on_stderr:(bytes -> int -> int -> unit) ->
  unit ->
  (unit -> 'a) ->
  ('a, exn) result * string * string
(** [with_capture_stdio_string () f] computes [f()] but captures stdout/stderr
    into strings.
    This captures a lock because stdout/stderr are global resources.
    @param on_stdout callback called when stdout is read. {b NOTE} this callback is called
    from a different thread.
    @param on_stderr same as [on_stdout] but for stderr.
    *)

val dup_stdin_stdout :
  ?on_stdin:(bytes -> int -> int -> int) ->
  ?on_stdout:(bytes -> int -> int -> unit) ->
  unit ->
  in_channel * out_channel
(** [dup_stdin_stdout ()] returns [cin, cout] where [cout] is a copy of
    the old stdout, and [cin] is a copy of the old [stdin],
    and [stdin] and [stdout] are redirected into the void (unless callbacks are provided).
    @param on_stdout a callback invoked every time something is printed on the new stdout
    @param on_stdin a callback invoked every time something needs to be read from the new stdin
*)

val mkdir_rec : string -> unit
(** Ensure directory exists, recursively *)
