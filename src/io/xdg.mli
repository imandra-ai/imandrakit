(** Basic XDG config

    We follow losely
    https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html *)

val get_home : unit -> string
(** Main home directory for the user. *)

val interpolate_home : ?f:(string -> string option) -> string -> string
(** Replace [$HOME] by the home directory in this string.
    @param f used to interpolate other keys
    @raise Failure if another key is found and not handled by [f]
*)

val config_dir : unit -> string
(** Where to search for configuration for this user. *)

val data_dir : unit -> string
(** Where to store permanent data for this user. *)

val cache_dir : unit -> string
(** Where to store disposable data that is only useful for improving perf
    but can be erased at any point. *)

val state_dir : unit -> string
(** Where to store state that is user-specific and can survive reboots. *)

val runtime_dir : unit -> string option
(** Where to store runtime files such as unix sockets.
    This might fail. *)

module type ARG = Xdg_sig.ARG
module type S = Xdg_sig.S

module Make (_ : ARG) : S
