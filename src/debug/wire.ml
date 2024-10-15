open Common_

let write (oc : out_channel) (j : Json.t) : unit =
  let s = Json.to_string j in
  (* Log.debug (fun k -> k "WRITE: %s" s); *)
  let s = Printf.sprintf "Content-Length: %d\r\n\r\n%s" (String.length s) s in
  Printf.fprintf oc "%s%!" s

open struct
  let rec read_headers ic acc =
    let line = input_line ic in
    if String.equal line "\r\n" || String.equal line "\r" then
      List.rev acc
    else (
      match String.index_opt line ':' with
      | None ->
        Error.failf ~kind:debug_error
          "Cannot read DAP header line, missing ':' in %S." line
      | Some i ->
        let k = String.sub line 0 i |> String.lowercase_ascii in
        let v = String.sub line (i + 1) (String.length line - i - 1) in
        read_headers ic ((k, v) :: acc)
    )
end

let read (ic : in_channel) : Json.t option Error.result =
  let@ () = Error.try_catch ~kind:debug_error () in
  match read_headers ic [] with
  | exception End_of_file -> None
  | hd ->
    let len =
      try
        let cl = List.assoc "content-length" hd in
        let cl = String.trim cl in
        match int_of_string_opt cl with
        | Some i -> i
        | None ->
          Log.err (fun k -> k "int_of_string_opt failed");
          0
      with Not_found ->
        Error.fail ~kind:debug_error "No content-length header"
    in
    let bs = Bytes.create len in
    really_input ic bs 0 len;

    (try
       let r = Json.from_string (Bytes.unsafe_to_string bs) in
       (* Log.debug (fun k -> k "READ: %s" (Json.to_string r)); *)
       Some r
     with Yojson.Json_error msg ->
       Error.failf ~kind:debug_error "Invalid JSON: %s" msg)
