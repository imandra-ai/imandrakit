(** Magic trace stop indicator.

    This magic function will, {b IF} magic-trace is attached to
    the current process, trigger a snapshot of the last few milliseconds.

    See https://github.com/janestreet/magic-trace for more details.
    Typical use:

    - [magic-trace run -multi-snapshot -trigger . -- foo.exe] (use this special symbol)
    - [magic-trace run -multi-snapshot -trigger '?' -- foo.exe] (to pick the symbol to stop)
    - [magic-trace attach <pid>]

    then use [trace-processor trace.fxt --httpd] to process the trace if it's too
    big to be processed in perfetto's web UI

 *)

val mark_start : unit -> unit
val trigger_snapshot : unit -> unit
val with_snapshot : (unit -> 'a) -> 'a
