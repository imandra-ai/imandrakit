module Log = (val Imandrakit_log.Logger.mk_log_str "x.popen")

type t = {
  pid: int;
  stdin: out_channel;
  stdout: in_channel;
  stderr: in_channel;
  mutable exit_code: (int, exn) result option;
  exit_code_mutex: Mutex.t;
  exit_code_condition: Condition.t;
  start_time: Ptime.t;
  mutable stop_time: Ptime.t option;
  mutable on_exit: (t -> (int, exn) result -> unit) list;
  is_group_leader: bool;
}

exception Killed

(* Processes that we started. Global because all threads need access to it. *)
let g_running_processes : t list ref = ref []
let g_running_processes_mtx : Mutex.t = Mutex.create ()
let g_more_to_reap : bool Atomic.t = Atomic.make false

(* Initialization flag. *)
let g_initialized : bool ref = ref false
let g_initialized_mtx : Mutex.t = Mutex.create ()

let fulfill (p : t) (r : (int, exn) result) (pid : int) : unit =
  Mutex.lock p.exit_code_mutex;
  p.exit_code <- Some r;
  p.stop_time <- Some (Ptime_clock.now ());
  Condition.broadcast p.exit_code_condition;
  Mutex.unlock p.exit_code_mutex;
  List.iter (fun f -> ignore (Thread.create (fun _ -> f p r))) p.on_exit

let reap_one (p : t) : bool =
  try
    let wpid, wstatus = Unix.waitpid [ WNOHANG ] p.pid in
    if wpid <> p.pid then
      true
    else (
      match wstatus with
      | WEXITED c ->
        Log.debug (fun k -> k "(resolve :ok %d)" p.pid);
        fulfill p (Ok c) p.pid;
        false
      | WSIGNALED c ->
        Log.debug (fun k -> k "(resolve :error %d)" p.pid);
        fulfill p (Error Killed) p.pid;
        false
      | WSTOPPED _ ->
        (* Unreachable without WUNTRACED. *)
        true
    )
  with Unix.Unix_error (Unix.ECHILD, _, _) -> true

let rec reap () =
  Atomic.set g_more_to_reap true;
  if Mutex.try_lock g_running_processes_mtx then (
    while Atomic.exchange g_more_to_reap false do
      try
        let r = List.filter reap_one !g_running_processes in
        g_running_processes := r;
        if List.length r <= 2 then
          Log.debug (fun k ->
              k "(@[remaining %a@])" (Fmt.Dump.list Fmt.int)
                (List.map (fun x -> x.pid) r))
        else
          Log.debug (fun k -> k "(@[remaining :n %d@])" (List.length r))
      with exc ->
        Log.debug (fun k ->
            k "(@[reap :exception '%s'@])" (Printexc.to_string exc))
    done;
    Mutex.unlock g_running_processes_mtx
  )

let init () =
  Mutex.protect g_initialized_mtx (fun _ ->
      if not !g_initialized then (
        let old_handler = Sys.signal Sys.sigchld Sys.Signal_ignore in

        ignore
          (Sys.set_signal Sys.sigchld
             (Sys.Signal_handle
                (fun _ ->
                  Log.debug (fun k -> k "(sigchld)");
                  reap ();
                  match old_handler with
                  | Sys.Signal_handle h -> h Sys.sigchld
                  | _ -> ())));

        ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [ Sys.sigchld ]);

        g_initialized := true
      ))

let spawn (is_group_leader : bool) (env : string array) (cmd : string)
    (args : string array) : t =
  init ();

  if not (String.equal Sys.os_type "Win32") then
    ignore (Unix.sigprocmask Unix.SIG_BLOCK [ Sys.sigpipe ]);

  (* Make pipes, to give the appropriate ends to the subprocess *)
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
  (* Close the subprocess ends in here *)
  Unix.close p_stdout;
  Unix.close p_stdin;
  Unix.close p_stderr;
  let r =
    {
      pid;
      stdin;
      stdout;
      stderr;
      exit_code = None;
      exit_code_mutex = Mutex.create ();
      exit_code_condition = Condition.create ();
      on_exit = [];
      start_time = Ptime_clock.now ();
      stop_time = None;
      is_group_leader;
    }
  in
  Log.debug (fun k -> k "(spawn :pid %d :cmd %S)" r.pid cmd);
  Mutex.protect g_running_processes_mtx (fun x ->
      g_running_processes := r :: !g_running_processes);
  r

let run ?(is_group_leader = false) ?(env = Unix.environment ()) (cmd : string)
    (args : string list) : t =
  spawn is_group_leader env cmd (Array.of_list (cmd :: args))

let pid_alive (pid : int) =
  try
    Unix.kill pid 0;
    true
  with Unix.Unix_error (Unix.ESRCH, _, _) -> false

let pid_is_gone ~(pid : int) ~(max_wait_s : float) =
  let deadline = Unix.gettimeofday () +. max_wait_s in
  let rec loop () =
    if not (pid_alive pid) then
      true
    else if Unix.gettimeofday () > deadline then
      false
    else (
      Unix.sleepf 0.1;
      loop ()
    )
  in
  loop ()

let await (self : t) : (int, exn) result =
  Log.debug (fun k -> k "(await %d)" self.pid);
  Mutex.lock self.exit_code_mutex;
  let r =
    match self.exit_code with
    | Some ec -> ec
    | None ->
      Condition.wait self.exit_code_condition self.exit_code_mutex;
      Option.value self.exit_code
        ~default:
          (Error (Failure "Exit code of process unexpectedly not present."))
  in
  Mutex.unlock self.exit_code_mutex;
  r

let kill ?(max_wait_s = 0.5) self =
  Log.debug (fun k -> k "(kill %d)" self.pid);
  let max_wait_s = max 0.0 max_wait_s in
  try
    let pgid =
      if self.is_group_leader then
        -self.pid
      else
        self.pid
    in

    (try Unix.kill pgid Sys.sigterm with
    | Unix.Unix_error (Unix.ESRCH, _, _) ->
      (* Perhaps it hasn't become a group leader yet. *)
      (try Unix.kill self.pid Sys.sigterm with
      | Unix.Unix_error (Unix.ESRCH, _, _) ->
        (* Perhaps it just became a group leader. *)
        (try Unix.kill pgid Sys.sigterm with _ -> ())
      | exc ->
        Log.debug (fun k ->
            k "(@[kill :exception1@ '%s'@])" (Printexc.to_string exc)))
    | exc ->
      Log.debug (fun k ->
          k "(@[kill :exception2@ '%s'@])" (Printexc.to_string exc)));

    if not (pid_is_gone ~pid:pgid ~max_wait_s:(max_wait_s *. 0.75)) then (
      Log.debug (fun k -> k "(hard-kill %d)" pgid);
      (try Unix.kill (-self.pid) Sys.sigkill with _ -> ());
      (try Unix.kill self.pid Sys.sigkill with _ -> ());
      if
        (not (pid_is_gone ~pid:pgid ~max_wait_s:(max_wait_s *. 0.25)))
        && max_wait_s <> 0.0
      then
        Log.warn (fun k ->
            k
              "Could not verify that PID %d was killed successfully; child \
               processes may be leaked."
              pgid)
    );

    reap ()
  with
  | Unix.Unix_error (Unix.ESRCH, _, _) -> (* Ok, nothing to kill *) ()
  | exc ->
    Log.warn (fun k ->
        k
          "Child processes may be leaked due to exception raised while \
           attempting to kill process %d: %s"
          self.pid (Printexc.to_string exc))

let kill_all () =
  Mutex.protect g_running_processes_mtx (fun x ->
      List.iter kill !g_running_processes)

let signal (self : t) (s : int) = Unix.kill self.pid s

let on_exit (self : t) (f : t -> (int, exn) result -> unit) : unit =
  self.on_exit <- f :: self.on_exit

let pid (self : t) : int = self.pid
let stdin (self : t) : out_channel = self.stdin
let stdout (self : t) : in_channel = self.stdout
let stderr (self : t) : in_channel = self.stderr
let start_time (self : t) : Ptime.t = self.start_time
let stop_time (self : t) : Ptime.t option = self.stop_time

let execution_time (self : t) : Ptime.span option =
  Option.map (fun x -> Ptime.diff x self.start_time) self.stop_time
