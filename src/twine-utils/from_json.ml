open Common_

module Conv (X : sig
  val string_sharing_threshold : int option
end) =
struct
  let string_sharing_threshold =
    Option.value ~default:4 X.string_sharing_threshold

  module Cached_string = struct
    type t = string

    let key = Twine.Encode.create_cache_key (module CCString)

    let to_twine_ enc s =
      if String.length s > string_sharing_threshold then (
        let p = Twine.Encode.write_immediate enc @@ Twine.Immediate.string s in
        Twine.Immediate.pointer p
      ) else
        Twine.Immediate.string s

    let to_twine enc (s : t) = Twine.Encode.with_cache key to_twine_ enc s
  end

  let rec json_to_twine (enc : Twine.Encode.t) (j : Json.t) : Twine.immediate =
    match j with
    | `Null -> Twine.Immediate.Null
    | `Bool b -> Twine.Immediate.bool b
    | `Int i -> Twine.Immediate.int i
    | `Intlit i ->
      (try
         let i = Int64.of_string i in
         Twine.Immediate.int64 i
       with _ -> Twine.Immediate.string i)
    | `Float f -> Twine.Immediate.float f
    | `String s -> Cached_string.to_twine enc s
    | `Tuple l | `List l ->
      Twine.Encode.array_iter enc
        (Iter.of_list l |> Iter.map (json_to_twine enc))
    | `Variant (s, None) -> Twine.Immediate.string s
    | `Variant (s, Some arg) ->
      Twine.Encode.array enc
        [| Twine.Immediate.string s; json_to_twine enc arg |]
    | `Assoc l ->
      Twine.Encode.dict_iter enc
        (Iter.of_list l
        |> Iter.map (fun (k, v) ->
               Cached_string.to_twine enc k, json_to_twine enc v))
end

let run (self : Commands.from_json) : unit =
  let@ () = Util.with_logging self.logging in
  Log.debug (fun k -> k "running %a" Commands.pp_from_json self);
  let content = Util.get_content self.input in
  Log.debug (fun k -> k "got %dB of content" (String.length content));
  let j =
    try Json.from_string content
    with exn ->
      Log.err (fun k -> k "Cannot parse JSON: %s" (Printexc.to_string exn));
      Util.exit_with 1
  in
  Log.debug (fun k -> k "got json");

  let twine =
    let module Conv = Conv (struct
      let string_sharing_threshold = self.string_sharing_threshold
    end) in
    Twine.Encode.encode_to_string Conv.json_to_twine j
  in
  Log.debug (fun k -> k "got %dB of twine" (String.length twine));

  let res =
    if self.dump then
      Twine.Dump.dump_string
        ?string_ellipsis_threshold:self.dump_string_ellipsis_threshold twine
    else if self.hexdump then
      Hex.hexdump_s (Hex.of_string twine)
    else if self.size then
      spf "%s\n" (IK.Util.format_byte_size @@ String.length twine)
    else if self.output.o_hex then
      CCString.to_hex twine
    else
      twine
  in

  match self.output.output_file with
  | Some file -> CCIO.File.write_exn file res
  | None ->
    output_string stdout res;
    flush stdout
