(** Codec (encoder/decoder) for values *)

module Twine = Imandrakit_twine

let error_kind : Imandrakit_error.Kind.t =
  Imandrakit_error.Kind.make ~name:"CodecErr" ()

type 'a t = {
  name: string;
  enc: 'a Twine.encoder;  (** Encode value *)
  dec: 'a Twine.decoder;  (** Decode value *)
}
(** A codec for values of type ['a] *)

let[@inline] name self = self.name
let[@inline] encode self enc x = self.enc enc x
let[@inline] decode (self : 'a t) dec (x : Twine.offset) : 'a = self.dec dec x
let create ~enc ~dec ~name () : _ t = { enc; dec; name }
