include Zip

let with_open_in file f =
  let ic = Zip.open_in file in
  Fun.protect ~finally:(fun () -> Zip.close_in ic) (fun () -> f ic)

let with_open_out file f =
  let oc = Zip.open_out file in
  Fun.protect ~finally:(fun () -> Zip.close_out oc) (fun () -> f oc)
