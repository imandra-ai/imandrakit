open Imandrakit_twine

type t = {
  a: int;
  b: string Imandrakit_twine.offset_for;
  c: string Imandrakit_twine.offset_for;
}
[@@deriving twine, show]

class twine_out_channel (oc : out_channel) =
  object
    method output bs i len = output oc bs i len
  end

let () =
  let tfn = Filename.temp_file "imandrakit." ".test" in
  Fun.protect ~finally:(fun _ -> Sys.remove tfn) @@ fun () ->
  (* Start a twine file with some immediate values *)
  let oc = open_out_bin tfn in
  let enc = Encode.create () in
  let out : Encode.out = new twine_out_channel oc in
  let _ = Encode.write_immediate enc (Immediate.string "UNUSED STUFF") in
  let off_xyz = Encode.write_immediate enc (Immediate.string "XYZ") in

  (* Write a twine blob containing references *)
  let t_out = { a = 43; b = Offset_for off_xyz; c = Offset_for off_xyz } in
  Format.printf "t_out: %a@." pp t_out;

  let entrypoint = to_twine enc t_out in
  let _ = Encode.finalize enc ~entrypoint in
  Imandrakit_twine.Encode.write_internal_data enc out;
  flush oc;
  close_out oc;

  (* Now read the back from twine file *)
  let ic = open_in_bin tfn in
  let dec = Decode.of_in_channel ic in
  let t_in = of_twine dec (Decode.get_entrypoint dec) in

  Format.printf "t_in : %a@." pp t_in;

  (* Make sure we can dereference correctly *)
  let b = Decode.read_ref dec Decode.string t_in.b in
  let c = Decode.read_ref dec Decode.string t_in.c in
  close_in ic;

  Format.printf "a=%d b=%s c=%s@." t_in.a b c;
  assert (String.equal b c && String.equal b "XYZ");
  assert (t_out = t_in);

  (* Dump the file so we get a test failure if the contents change *)
  let ic = open_in_bin tfn in
  let n = Int64.to_int (In_channel.length ic) in
  let bs = Bytes.create n in
  really_input ic bs 0 n;
  Hex.hexdump (Hex.of_bytes bs)
