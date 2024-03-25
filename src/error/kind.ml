type t = { name: string } [@@unboxed] [@@deriving show, ord, eq]

let make ~name () : t = { name }

let codec : t Codec.t =
  Codec.create ~name:"error_kind"
    ~enc:(fun { name } -> Value.string name)
    ~dec:
      Decode.(
        let+ name = string in
        make ~name ())
    ()

let generic_internal_error : t = make ~name:"GenericInternalError" ()
