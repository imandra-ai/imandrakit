(** Codec (encoder/decoder) for values *)

type 'a t = {
  name: string;
  enc: 'a -> Value.t;  (** Encode value *)
  dec: 'a Decode.t;  (** Decode value *)
}
(** A codec for values of type ['a] *)

let[@inline] name self = self.name
let[@inline] encode self x = self.enc x
let[@inline] decode self x = Decode.run self.dec x
let create ~enc ~dec ~name () : _ t = { enc; dec; name }
