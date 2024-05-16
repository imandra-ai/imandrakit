open Common_

type cli =
  | Dump of Commands.dump
  | From_json of Commands.from_json
[@@deriving subliner]

let run (cli : cli) : unit =
  let@ () = Util.top_exit_handler () in
  let@ () = Trace_tef.with_setup () in
  match cli with
  | Dump d -> Dump.run d
  | From_json j -> From_json.run j

[%%subliner.cmds eval.cli <- run] [@@name "twine-utils"]
