include Gzip

let with_open_in file f =
  let ic = Gzip.open_in file in
  Fun.protect ~finally:(fun () -> Gzip.close_in ic) (fun () -> f ic)

let with_open_out file f =
  let oc = Gzip.open_out file in
  Fun.protect ~finally:(fun () -> Gzip.close_out oc) (fun () -> f oc)

let output_string oc s : unit = output_substring oc s 0 (String.length s)
