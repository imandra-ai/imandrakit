module type ARG = sig
  val project_name : string
end

module type S = sig
  val default_storage_dir : unit -> string
  (** Default storage dir for the given project *)

  val default_runtime_dir : unit -> string
  (** Default (runtime) storage for the given project. State might not survive reboot. *)
end
