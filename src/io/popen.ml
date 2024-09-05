open Moonpool
module Fd = Moonpool_io.Fd
module Log = (val Imandrakit_log.Logger.mk_log_str "x.popen")

type state = {
  stopped: bool Atomic.t;
  res_code: int Fut.t;
  promise_code: int Fut.promise;
}

type t = {
  stdin: Utils.Output.t;
  stdout: Utils.Input.t;
  stderr: Utils.Input.t;
  pid: int;
  _st: state;
}
(** A sub-process *)

let pp out self = Fmt.fprintf out "<pid=%d>" self.pid
let show self = spf "<pid=%d>" self.pid
let[@inline] stopped self = Atomic.get self._st.stopped

let waitpid_int_ (pid : int) : int =
  match Unix.waitpid [ Unix.WUNTRACED ] pid with
  | _, (Unix.WEXITED i | Unix.WSTOPPED i | Unix.WSIGNALED i) -> i
  | exception _ -> 0

let waitpid_int_nonblock_ (pid : int) : int =
  (* non blocking wait *)
  match Moonpool_io.Unix.waitpid [ Unix.WUNTRACED ] pid with
  | _, (Unix.WEXITED i | Unix.WSTOPPED i | Unix.WSIGNALED i) -> i
  | exception _ -> 0

let kill_and_close_ (self : t) =
  let already_stopped = Atomic.exchange self._st.stopped true in
  if not already_stopped then (
    Log.debug (fun k -> k "kill/close subprocess pid=%d" self.pid);
    (try Unix.kill self.pid 15 with _ -> ());
    Utils.Output.close self.stdin;
    Utils.Input.close self.stdout;
    Utils.Input.close self.stderr;

    let runner = Moonpool.Runner.get_current_runner () in

    (* just to be sure, wait a second and kill dash nine *)
    (match runner with
    | None ->
      ignore
        (Thread.create
           (fun () ->
             Thread.delay 1.;
             try Unix.kill self.pid 9 with _ -> ())
           ()
          : Thread.t)
    | Some runner ->
      ignore
        (Moonpool_fib.spawn_top ~on:runner (fun () ->
             Moonpool_io.Unix.sleepf 1.;
             try Moonpool_io.Unix.kill self.pid 9 with _ -> ())
          : _ Moonpool_fib.t));

    (* kill zombies *)
    match runner with
    | None ->
      ignore
        (Thread.create
           (fun () ->
             let code = try waitpid_int_ self.pid with _ -> max_int in
             Fut.fulfill_idempotent self._st.promise_code @@ Ok code)
           ()
          : Thread.t)
    | Some runner ->
      ignore
        (Moonpool_fib.spawn_top ~on:runner (fun () ->
             let code =
               try waitpid_int_nonblock_ self.pid with _ -> max_int
             in
             Fut.fulfill_idempotent self._st.promise_code @@ Ok code)
          : _ Moonpool_fib.t)
  )

let run_ ?(env = Unix.environment ()) cmd args : t =
  (* block sigpipe *)
  ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ]);
  (* make pipes, to give the appropriate ends to the subprocess *)
  let stdout, p_stdout = Unix.pipe () in
  let stderr, p_stderr = Unix.pipe () in
  let p_stdin, stdin = Unix.pipe () in
  (* close our ends in the subprocess *)
  Unix.set_close_on_exec stdout;
  Unix.set_close_on_exec stderr;
  Unix.set_close_on_exec stdin;
  let stdout =
    new Utils.Input.of_unix_fd ~buf:(Utils.Slice.create 512) @@ Fd.create stdout
  in
  let stderr =
    new Utils.Input.of_unix_fd ~buf:(Utils.Slice.create 512) @@ Fd.create stderr
  in
  let stdin =
    new Utils.Output.of_unix_fd ~buf:(Utils.Slice.create 512) @@ Fd.create stdin
  in
  let pid = Unix.create_process_env cmd args env p_stdin p_stdout p_stderr in
  let res_code, promise_code = Fut.make () in
  Log.debug (fun k ->
      k "opened subprocess pid=%d cmd=%S args=[…%d]" pid cmd (Array.length args));
  (* close the subprocess ends in here *)
  Unix.close p_stdout;
  Unix.close p_stdin;
  Unix.close p_stderr;
  let p =
    {
      stdin;
      stdout;
      stderr;
      pid;
      _st = { stopped = Atomic.make false; res_code; promise_code };
    }
  in
  Gc.finalise kill_and_close_ p;
  p

let run ?env cmd args : t = run_ ?env cmd (Array.of_list (cmd :: args))
let res_code self = self._st.res_code
let run_shell ?env cmd : t = run_ ?env "/bin/sh" [| "/bin/sh"; "-c"; cmd |]
let kill self = kill_and_close_ self
let signal self s = Unix.kill self.pid s

let wait_block (self : t) : int =
  let res = waitpid_int_ self.pid in
  kill_and_close_ self;
  res

let await (self : t) : int =
  let res = waitpid_int_nonblock_ self.pid in
  kill_and_close_ self;
  res
