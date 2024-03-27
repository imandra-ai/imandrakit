(** Supervisor-like features *)

val retry_loop :
  ?max:int ->
  ?initial_delay_before_restart_s:float ->
  ?max_delay_before_restart_s:float ->
  (unit -> 'a) ->
  'a
(** [retry_loop f] runs [f()]; if [f()] fails, the loop waits
    a bit and tries again.
    @param max_delay_before_restart_s upper bound on the delay. Default 60s.
    @param max after this many attempts, the last failure of [f()] is
    raised again and the whole loop fails. Default [max_int]. *)
