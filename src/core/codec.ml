(** Codec (encoder/decoder) for values *)

module V = Imandrakit_ser.Value

let error_kind : Imandrakit_error.Kind.t =
  Imandrakit_error.Kind.make ~name:"CodecErr" ()

type 'a t = {
  name: string;
  enc: 'a -> V.t;  (** Encode value *)
  dec: V.t -> 'a;  (** Decode value *)
}
(** A codec for values of type ['a] *)

let[@inline] name self = self.name
let[@inline] encode self x : V.t = self.enc x
let[@inline] decode (self : 'a t) (x : V.t) : 'a = self.dec x
let create ~enc ~dec ~name () : _ t = { enc; dec; name }

(** A codec using ser-pack for (de)serialization *)
let create_serpack ~ser ~deser ~name () : _ t =
  let enc x = Imandrakit_ser_pack.to_value ser x in
  let dec x =
    match Imandrakit_ser_pack.of_value deser x with
    | Ok v -> v
    | Error msg -> Imandrakit_error.Error.fail ~kind:error_kind msg
  in
  create ~enc ~dec ~name ()
