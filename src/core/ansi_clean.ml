(* reference:
   https://en.wikipedia.org/wiki/ANSI_escape_code
*)

let esc = '\x1b'

type tok =
  | Char of char
  | Escape of string
  | Err of {
      i: int;
      msg: string;
    }
  | End

module Parser : sig
  type t

  val make : string -> t
  val next : t -> tok
end = struct
  type t = {
    s: string;
    mutable i: int;
  }

  let make s = { s; i = 0 }
  let[@inline] eof_ self = self.i >= String.length self.s
  let junk_ self = self.i <- self.i + 1

  let get_or_err_ self =
    if eof_ self then raise End_of_file;
    let c = String.get self.s self.i in
    junk_ self;
    c

  let next (self : t) : tok =
    let exception Bad_char of char in
    try
      if eof_ self then
        End
      else (
        let i0 = self.i in
        let c = String.get self.s self.i in
        junk_ self;

        if c = esc && get_or_err_ self = '[' then (
          (* parsed CSI *)
          let continue = ref true in
          while !continue do
            let c = get_or_err_ self in
            match c with
            | '\x30' .. '\x3F' -> () (* params *)
            | '\x20' .. '\x2f' -> () (* intermediate bytes *)
            | '\x40' .. '\x7e' -> continue := false (* final byte *)
            | c -> raise (Bad_char c)
          done;

          let sub = String.sub self.s i0 (self.i - i0) in
          Escape sub
        ) else
          Char c
      )
    with
    | End_of_file -> Err { i = self.i; msg = "unexpected end of input" }
    | Bad_char c ->
      Err
        {
          i = self.i;
          msg = Printf.sprintf "invalid char in escape sequence: %C" c;
        }
end

let remove_escape_codes (s : string) : string =
  if String.contains s esc then (
    let buf = Buffer.create (String.length s) in

    let continue = ref true in
    let p = Parser.make s in
    while !continue do
      match Parser.next p with
      | End -> continue := false
      | Escape _ -> () (* skip *)
      | Err _ -> () (* skip *)
      | Char c -> Buffer.add_char buf c
    done;

    Buffer.contents buf
  ) else
    s
