module String_map = Map.Make (String)

let print_exn_at_loc ~__FILE__ ~__LINE__ =
  Printf.sprintf "Exception raised at %s:%d" __FILE__ __LINE__
