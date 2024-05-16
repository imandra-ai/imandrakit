open Common_

open struct
  exception Exit of int
end

(** Exit program with [i] *)
let exit_with i = raise @@ Exit i

(** Outermost handler for exits *)
let top_exit_handler () f = try f () with Exit i -> exit i

let with_logging (conf : Commands.logging) f =
  let log_level =
    if conf.quiet then
      IK.Log_level.Error
    else if conf.debug then
      IK.Log_level.Debug
    else
      IK.Log_level.Info
  in
  IK.Logger.setup_level ~log_level ();

  match conf.log_file with
  | None when conf.quiet -> f ()
  | None ->
    IK.Logger.setup_logger_to_stderr ();
    let@ () = Fun.protect ~finally:IK.Logger.fence in
    f ()
  | Some filename -> IK.Logger.setup_logger_to_LOG_FILE ~filename () f

let get_content (i : Commands.input) : string =
  let file = i.file in
  let s =
    if file = "-" then
      CCIO.read_all stdin
    else (
      try CCIO.File.read_exn file
      with exn ->
        Log.err (fun k ->
            k "cannot read file %S: %s" file (Printexc.to_string exn));
        exit_with 1
    )
  in

  if i.i_hex then (
    match CCString.of_hex s with
    | Some s -> s
    | None ->
      Log.err (fun k -> k "Invalid hex content");
      exit_with 1
  ) else
    s
