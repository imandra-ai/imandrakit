type 'a eq = 'a -> 'a -> bool
type 'a print = 'a -> string

module Test : sig
  type t
end

module type S = sig
  module Q = QCheck

  val t : ?name:string -> (unit -> bool) -> unit
  val eq : ?name:string -> ?cmp:'a eq -> ?printer:'a print -> 'a -> 'a -> unit
  val neq : ?name:string -> ?cmp:'a eq -> ?printer:'a print -> 'a -> 'a -> unit

  val q :
    ?name:string ->
    ?count:int ->
    ?long_factor:int ->
    ?max_gen:int ->
    ?max_fail:int ->
    ?if_assumptions_fail:[ `Fatal | `Warning ] * float ->
    'a Q.arbitrary ->
    ('a -> bool) ->
    unit

  val assert_equal :
    ?msg:string ->
    ?printer:('a -> string) ->
    ?cmp:('a -> 'a -> bool) ->
    'a ->
    'a ->
    unit

  val assert_bool : string -> bool -> unit
  val assert_failure : string -> 'a
  val assert_raises : (exn -> bool) -> (unit -> 'b) -> unit
  val get : unit -> Test.t list
end

val make : __FILE__:string -> unit -> (module S)

val run_all :
  ?seed:string -> ?long:bool -> descr:string -> Test.t list list -> unit
