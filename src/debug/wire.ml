open Common_

let write (oc : out_channel) (j : Json.t) : unit =
  let s = Json.to_string j in
  Printf.fprintf oc "content-length: %d\r\n\r\n%s%!" (String.length s) s

open struct
  let rec read_headers ic acc =
    let line = input_line ic in
    if line = "\r\n" then
      List.rev acc
    else (
      match String.index_opt line ':' with
      | None ->
        Error.failf ~kind:debug_error "Cannot read DAP line, missing ':' in %S."
          line
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
      try List.assoc "content-length" hd |> int_of_string
      with _ -> Error.fail ~kind:debug_error "No content-length header"
    in
    let bs = Bytes.create len in
    really_input ic bs 0 len;
    (try Some (Json.from_string (Bytes.unsafe_to_string bs))
     with Yojson.Json_error msg ->
       Error.failf ~kind:debug_error "Invalid JSON: %s" msg)
