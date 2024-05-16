open Common_

let run (self : Commands.dump) : unit =
  let@ () = Util.with_logging self.logging in
  let content = Util.get_content self.input in

  if self.hexdump then
    Hex.hexdump (Hex.of_string content)
  else (
    let dumped = Twine.Dump.dump_string content in
    output_string stdout dumped;
    flush stdout
  )
