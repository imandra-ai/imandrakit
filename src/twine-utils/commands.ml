(** CLI commands *)

type logging = {
  debug: bool; [@names [ "d"; "debug" ]]  (** debug *)
  log_file: string option;
  quiet: bool; [@names [ "q"; "quiet" ]]
}
[@@deriving show { with_path = false }, subliner]

type input = {
  file: string; [@pos 0]  (** The file to parse *)
  i_hex: bool;  (** Read from hex? *)
}
[@@deriving show { with_path = false }, subliner]

type output = {
  output_file: string option; [@names [ "o"; "output" ]]
      (** Write output into this file *)
  o_hex: bool;  (** Dump as hex? *)
}
[@@deriving show { with_path = false }, subliner]

type dump = {
  input: input; [@term input_cmdliner_term ()]
  logging: logging; [@term logging_cmdliner_term ()]
  hexdump: bool;  (** Dump as hex *)
}
[@@deriving show { with_path = false }, subliner]

type from_json = {
  input: input; [@term input_cmdliner_term ()]
  logging: logging; [@term logging_cmdliner_term ()]
  string_sharing_threshold: int option;
      (** String strictly above this size will be interned to enable sharing *)
  dump_string_ellipsis_threshold: int option;  (** Control ellipsis in dump *)
  output: output; [@term output_cmdliner_term ()]
  no_finalizer: bool;  (** Skip writing the finalizer block *)
  dump: bool;  (** Result is a dump of the resulting twine blob *)
  size: bool;  (** Result is the byte size of the resulting twine blob*)
  hexdump: bool;  (** Result is a hexdump of the resulting twine blob *)
}
[@@deriving show { with_path = false }, subliner]
