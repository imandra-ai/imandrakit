open Moonpool
module Log = (val Logger.mk_log_str "x.popen")

type state = {
  stopped: bool Atomic.t;
  res_code: int Fut.t;
  promise_code: int Fut.promise;
}

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
    close_out_noerr self.stdin;
    close_in_noerr self.stdout;
    close_in_noerr self.stderr;
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
  let stdout = Unix.in_channel_of_descr stdout in
  let stderr = Unix.in_channel_of_descr stderr in
  let stdin = Unix.out_channel_of_descr stdin in
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

let wait (self : t) : int =
  let _, res = Unix.waitpid [ Unix.WUNTRACED ] self.pid in
  kill_and_close_ self;
  let res =
    match res with
    | Unix.WEXITED i | Unix.WSTOPPED i | Unix.WSIGNALED i -> i
  in
  res
