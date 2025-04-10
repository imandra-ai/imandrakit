(** CBOR encoder/decoder for {!Imandrakit_ser}

    See {{:https://www.rfc-editor.org/rfc/rfc8949.html} the RFC}. *)

type t = Imandrakit_ser.Value.t

exception Error of string

val encode : ?buf:Buffer.t -> t -> string
val decode : string -> (t, string) result

val decode_exn : string -> t
(** Like {!decode}.
    @raise Error if the string isn't valid *)

val pp_diagnostic : t Fmt.printer
val show_diagnostic : t -> string
