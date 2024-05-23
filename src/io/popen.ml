open Moonpool
module Log = (val Logger.mk_log_str "x.popen")

type state = {
  stopped: bool Atomic.t;
  res_code: int Fut.t;
  promise_code: int Fut.promise;
  close_stdin: bool;
  close_stdout: bool;
  close_stderr: bool;
}

type redirect =
  [ `Keep
  | `Pipe
  ]

type t = {
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
  pid: int;
  _st: state;
}
(** A sub-process *)

let pp out self = Fmt.fprintf out "<pid=%d>" self.pid
let show self = spf "<pid=%d>" self.pid

let kill_and_close_ (self : t) =
  let already_stopped = Atomic.exchange self._st.stopped true in
  if not already_stopped then (
    Log.debug (fun k -> k "kill/close subprocess pid=%d" self.pid);
    (try Unix.kill self.pid 15 with _ -> ());
    if self._st.close_stdin then close_out_noerr self.stdin;
    if self._st.close_stdout then close_in_noerr self.stdout;
    if self._st.close_stderr then close_in_noerr self.stderr;
    (* just to be sure, wait a second and kill dash nine *)
    ignore
      (Thread.create
         (fun () ->
           Thread.delay 1.;
           try Unix.kill self.pid 9 with _ -> ())
         ()
        : Thread.t);

    (* kill zombies *)
    let code = try fst @@ Unix.waitpid [] self.pid with _ -> max_int in
    Fut.fulfill_idempotent self._st.promise_code @@ Ok code
  )

let run_ ?(env = Unix.environment ()) ?stdin:(r_stdin = `Pipe)
    ?stdout:(r_stdout = `Pipe) ?stderr:(r_stderr = `Pipe) cmd args : t =
  (* block sigpipe *)
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ]);

  let mkpipe (r : [< redirect ]) input output =
    match r with
    | `Keep -> input, output, false
    | `Pipe ->
      let ic, oc = Unix.pipe () in
      ic, oc, true
  in

  (* make pipes, to give the appropriate ends to the subprocess *)
  let stdout, p_stdout, cleanup_stdout =
    mkpipe r_stdout Unix.stdin Unix.stdout
  in
  let stderr, p_stderr, cleanup_stderr =
    mkpipe r_stderr Unix.stdin Unix.stderr
  in
  let stdin, p_stdin, cleanup_stdin = mkpipe r_stdin Unix.stdout Unix.stdin in
  (* close our ends in the subprocess *)
  if cleanup_stdout then Unix.set_close_on_exec stdout;
  if cleanup_stderr then Unix.set_close_on_exec stderr;
  if cleanup_stdin then Unix.set_close_on_exec stdin;
  let stdout = Unix.in_channel_of_descr stdout in
  let stderr = Unix.in_channel_of_descr stderr in
  let stdin = Unix.out_channel_of_descr stdin in
  let pid = Unix.create_process_env cmd args env p_stdin p_stdout p_stderr in
  let res_code, promise_code = Fut.make () in
  Log.debug (fun k ->
      k "opened subprocess pid=%d cmd=%S args=[…%d]" pid cmd (Array.length args));
  (* close the subprocess-side channels in here *)
  if cleanup_stdout then Unix.close p_stdout;
  if cleanup_stdin then Unix.close p_stdin;
  if cleanup_stderr then Unix.close p_stderr;
  let p =
    {
      stdin;
      stdout;
      stderr;
      pid;
      _st =
        {
          stopped = Atomic.make false;
          res_code;
          promise_code;
          close_stdout = cleanup_stdout;
          close_stdin = cleanup_stdin;
          close_stderr = cleanup_stderr;
        };
    }
  in
  Gc.finalise kill_and_close_ p;
  p

let run ?env ?stdin ?stdout ?stderr cmd args : t =
  run_ ?env ?stdin ?stdout ?stderr cmd (Array.of_list (cmd :: args))

let res_code self = self._st.res_code

let run_shell ?env ?stdin ?stdout ?stderr cmd : t =
  run_ ?env ?stdin ?stdout ?stderr "/bin/sh" [| "/bin/sh"; "-c"; cmd |]

let kill self = kill_and_close_ self
let signal self s = Unix.kill self.pid s

let wait (self : t) : int =
  let res =
    try snd @@ Unix.waitpid [ Unix.WUNTRACED ] self.pid
    with _ -> Unix.WEXITED 0
  in
  kill_and_close_ self;
  let res =
    match res with
    | Unix.WEXITED i | Unix.WSTOPPED i | Unix.WSIGNALED i -> i
  in
  res
