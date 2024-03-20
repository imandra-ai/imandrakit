let getenv_or_empty s = try Sys.getenv s with _ -> ""

let ( <+> ) x y =
  if x = "" then
    y ()
  else
    x

let get_home : unit -> string =
  let s = lazy (getenv_or_empty "HOME" <+> fun () -> "/tmp") in
  fun () -> Lazy.force s

let interpolate_home ?(f = fun _ -> None) s =
  let buf = Buffer.create (String.length s) in
  Buffer.add_substitute buf
    (function
      | "HOME" | "home" -> get_home ()
      | s ->
        (match f s with
        | Some u -> u
        | None ->
          failwith
          @@ spf
               "Xdg: interpolating home directory: couldn't find variable: \
                '%s'."
               s))
    s;
  Buffer.contents buf

let config_dir () =
  getenv_or_empty "XDG_CONFIG_HOME" <+> fun () -> get_home () ^ "/.config"

let data_dir () =
  getenv_or_empty "XDG_DATA_HOME" <+> fun () -> get_home () ^ "/.local/share/"

let state_dir () =
  getenv_or_empty "XDG_STATE_HOME" <+> fun () -> get_home () ^ "/.local/state/"

let cache_dir () =
  getenv_or_empty "XDG_CACHE_HOME" <+> fun () -> get_home () ^ "/.cache/"

let runtime_dir () = Sys.getenv_opt "XDG_RUNTIME_DIR"

module type ARG = Xdg_sig.ARG
module type S = Xdg_sig.S

module Make (A : ARG) : S = struct
  let default_storage_dir () = Filename.concat (state_dir ()) A.project_name

  let default_runtime_dir () =
    match runtime_dir () with
    | Some d -> Filename.concat d A.project_name
    | None -> Filename.concat (cache_dir ()) A.project_name
end
