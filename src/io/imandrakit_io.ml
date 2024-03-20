module Popen = Popen
module Xdg = Xdg

let str_of_file = CCIO.File.read_exn
let write_to_file filename s = CCIO.File.write_exn filename s

type resolved_file = string

let[@inline] is_relative_ f =
  try Filename.is_relative f with Sys_error _ -> false

let normalize_in_mem_ p : string =
  assert (not (is_relative_ p));
  let ps = CCString.split ~by:Filename.dir_sep p in
  let skip, i =
    CCList.fold_right
      (fun p (skip, ps) ->
        assert (skip >= 0);
        match p, skip with
        | ("." | ""), _ -> skip, ps
        | "..", n -> n + 1, ps
        | _, 0 -> 0, p :: ps
        | _, n -> n - 1, ps)
      ps (0, [])
  in
  let pre = CCList.replicate skip ".." in
  let path = String.concat Filename.dir_sep (pre @ i) in
  path

(** Purely syntactic normalization *)
let make_absolute_in_memory ?(in_dir = "/") (file_name : string) : string =
  let f =
    if not (try Filename.is_relative file_name with Sys_error _ -> false) then
      file_name
    else (
      assert (not (is_relative_ in_dir));
      Filename.concat in_dir file_name
    )
  in
  normalize_in_mem_ f

let make_absolute ?(in_dir = Sys.getcwd ()) file_name =
  let f =
    if not (is_relative_ file_name) then
      file_name
    else
      Filename.concat in_dir file_name
  in
  try Unix.realpath f
  with Unix.Unix_error (Unix.ENOENT, _, _) -> normalize_in_mem_ f

let find_file ?(in_dir = Sys.getcwd ()) ~file_name ~load_path () :
    resolved_file option =
  let concat = Filename.concat in
  if Sys.file_exists file_name then
    Some (make_absolute ~in_dir file_name)
  else
    CCList.find_map
      (fun d ->
        let name = concat d file_name in
        if Sys.file_exists name then
          Some (make_absolute ~in_dir name)
        else
          None)
      load_path

let get_pid = Unix.getpid

let with_signal ?(signal = Sys.sigint) ~on_sig f =
  Sys.catch_break false;
  let handler = Sys.signal signal (Sys.Signal_handle on_sig) in
  let old_mask = Thread.sigmask Unix.SIG_UNBLOCK [ signal ] in
  Fun.protect f ~finally:(fun () ->
      Sys.set_signal signal handler;
      ignore (Thread.sigmask Unix.SIG_BLOCK old_mask : _ list);
      Sys.catch_break true)

let block_sigpipe_sigint () =
  ignore (Thread.sigmask Unix.SIG_BLOCK [ Sys.sigint; Sys.sigpipe ] : _ list)

let read_i32_framed ic : string =
  let buf_len = Bytes.create 4 in
  really_input ic buf_len 0 4;
  let len = Bytes.get_int32_le buf_len 0 |> Int32.to_int in
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  Bytes.unsafe_to_string buf

let write_i32_framed oc s : unit =
  let buf_len = Bytes.create 4 in
  if String.length s > Int32.to_int Int32.max_int then
    invalid_arg "write_i32_framed: message is too long";
  Bytes.set_int32_le buf_len 0 (Int32.of_int (String.length s));
  output oc buf_len 0 4;
  output_string oc s

(* make sure capturing IO is done in only one thread at a time *)
let mutex_capture_io_ = Mutex.create ()

let with_capture_stdio_file ~file ?(redirect_stdout = true)
    ?(redirect_stderr = true) ~append () f =
  if (not redirect_stderr) && not redirect_stdout then
    f ()
  else (
    Mutex.lock mutex_capture_io_;
    let@ () = Fun.protect ~finally:(fun () -> Mutex.unlock mutex_capture_io_) in

    (* Redirect stderr and stdout to some log file *)
    let oldstderr =
      if redirect_stderr then
        Unix.dup ~cloexec:true Unix.stderr
      else
        Unix.stderr
    in
    let oldstdout =
      if redirect_stdout then
        Unix.dup ~cloexec:true Unix.stdout
      else
        Unix.stdout
    in
    let out = ref None in
    (try
       let app_or_trunc =
         if append then
           Open_append
         else
           Open_trunc
       in
       let new_out =
         open_out_gen [ Open_creat; app_or_trunc; Open_wronly ] 0o644 file
       in
       out := Some new_out;
       if redirect_stderr then
         Unix.dup2 ~cloexec:true (Unix.descr_of_out_channel new_out) Unix.stderr;
       if redirect_stdout then
         Unix.dup2 ~cloexec:true (Unix.descr_of_out_channel new_out) Unix.stdout
     with _ -> ());
    let@ () =
      Fun.protect ~finally:(fun () ->
          flush stderr;
          flush stdout;
          if redirect_stderr then Unix.dup2 ~cloexec:true oldstderr Unix.stderr;
          if redirect_stdout then Unix.dup2 ~cloexec:true oldstdout Unix.stdout;
          Option.iter (fun oc -> try close_out_noerr oc with _ -> ()) !out)
    in
    f ()
  )

let delay_poll_ = 0.005

(** Read from the FD (a pipe's read end) and call [cb] on each chunk.
      Stops when [stop] is true. *)
let read_fd_ ~cb (stop : bool Atomic.t) (fd : Unix.file_descr) : unit =
  block_sigpipe_sigint ();
  let buf = Bytes.create 1024 in
  let continue = ref true in
  try
    Unix.set_nonblock fd;
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

    (* wait just a bit for something to happen on [fd] *)
    let wait () : unit =
      ignore (Unix.select [ fd ] [] [ fd ] delay_poll_ : _ * _ * _)
    in

    while !continue do
      (match Unix.read fd buf 0 (Bytes.length buf) with
      | n ->
        if n = 0 then
          continue := false
        else
          cb buf 0 n
      | exception Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) ->
        wait ());
      if Atomic.get stop then continue := false
    done
  with Unix.Unix_error (_, _, _) -> ()

let catch_ignore f = try f () with _ -> ()
let _cb_ignore _ _ _ = ()

let with_capture_stdio_string ?(on_stdout = _cb_ignore)
    ?(on_stderr = _cb_ignore) () f =
  Mutex.lock mutex_capture_io_;
  let@ () = Fun.protect ~finally:(fun () -> Mutex.unlock mutex_capture_io_) in
  (* Redirect stderr and stdout to some log file *)
  let oldstderr = Unix.dup ~cloexec:true Unix.stderr in
  let oldstdout = Unix.dup ~cloexec:true Unix.stdout in

  let newstdout_read, newstdout = Unix.pipe ~cloexec:true () in
  Unix.dup2 ~cloexec:true newstdout Unix.stdout;
  let newstderr_read, newstderr = Unix.pipe ~cloexec:true () in
  Unix.dup2 ~cloexec:true newstderr Unix.stderr;

  let buf_stdout = Buffer.create 32 in
  let buf_stderr = Buffer.create 32 in
  let stop = Atomic.make false in

  let t1 =
    Thread.create
      (fun () ->
        Trace.set_thread_name "stdout_cap_read";
        read_fd_
          ~cb:(fun b i len ->
            Buffer.add_subbytes buf_stdout b i len;
            on_stdout b i len)
          stop newstdout_read)
      ()
  in
  let t2 =
    Thread.create
      (fun () ->
        Trace.set_thread_name "stderr_cap_read";
        read_fd_
          ~cb:(fun b i len ->
            Buffer.add_subbytes buf_stderr b i len;
            on_stderr b i len)
          stop newstderr_read)
      ()
  in

  let cleanup () : string * string =
    flush stderr;
    flush stdout;

    catch_ignore (fun () -> Unix.close newstdout);
    catch_ignore (fun () -> Unix.close newstderr);

    (* stop the threads after a brief delay *)
    Thread.delay delay_poll_;
    Atomic.set stop true;

    Thread.join t1;
    Thread.join t2;

    (* close pipes *)
    catch_ignore (fun () -> Unix.close newstdout_read);
    catch_ignore (fun () -> Unix.close newstderr_read);

    (* restore previous FDs *)
    Unix.dup2 ~cloexec:true oldstderr Unix.stderr;
    Unix.dup2 ~cloexec:true oldstdout Unix.stdout;
    Unix.close oldstderr;
    Unix.close oldstdout;

    let out = Buffer.contents buf_stdout in
    let err = Buffer.contents buf_stderr in

    out, err
  in

  match f () with
  | res ->
    let out, err = cleanup () in
    Ok res, out, err
  | exception exn ->
    let out, err = cleanup () in
    Error exn, out, err

let dup_stdin_stdout ?on_stdin ?(on_stdout = _cb_ignore) () :
    in_channel * out_channel =
  let oldstdout = Unix.dup ~cloexec:true Unix.stdout in
  let oldstdin = Unix.dup ~cloexec:true Unix.stdin in

  let _stop = Atomic.make false in

  let newstdin, newstdin_write = Unix.pipe ~cloexec:true () in
  Unix.dup2 ~cloexec:true newstdin Unix.stdin;
  let newstdout_read, newstdout = Unix.pipe ~cloexec:true () in
  Unix.dup2 ~cloexec:true newstdout Unix.stdout;

  (* spawn threads to consume from the pipes. Even if callbacks are not specified,
     we need to drain the pipe so it doesn't block. *)
  let _tout =
    Thread.create
      (fun () ->
        Trace.set_thread_name "stdout_read";
        read_fd_ _stop newstdout_read ~cb:(fun b i len -> on_stdout b i len))
      ()
  in

  (* custom reader in a thread. Calls [read_stdin] and forwards what it reads
     into the write end of newstdin. *)
  (match on_stdin with
  | None -> Unix.close newstdin_write
  | Some read_stdin ->
    ignore
      (Thread.create
         (fun () ->
           Trace.set_thread_name "stdin_read";
           block_sigpipe_sigint ();

           let buf = Bytes.create 1024 in
           let continue = ref true in
           while !continue && not (Atomic.get _stop) do
             (* read from the callback *)
             let len = read_stdin buf 0 (Bytes.length buf) in
             if len = 0 then (
               continue := false;
               Unix.close newstdin_write
             ) else (
               (* write to the pipe *)
               let n_wr = ref 0 in
               while !n_wr < len do
                 let n_wr' =
                   Unix.write newstdin_write buf !n_wr (len - !n_wr)
                 in
                 n_wr := !n_wr + n_wr'
               done
             )
           done)
         ()
        : Thread.t));

  Unix.in_channel_of_descr oldstdin, Unix.out_channel_of_descr oldstdout

(** Ensure directory exists, recursively *)
let rec mkdir_rec (d : string) =
  if not (Sys.file_exists d) then (
    let d2 = Filename.dirname d in
    mkdir_rec d2;
    try Unix.mkdir d 0o755
    with _ -> Logs.debug (fun k -> k "mkdir %S failed" d)
  )
