open struct
  module MIO = Moonpool_io
  module Fd = Moonpool_io.Fd
end

module Slice = Iostream.Slice

(** Buffered output *)
module Output = struct
  include Iostream.Out_buf

  class of_unix_fd ?(close_noerr = false) ~(buf : Slice.t) (fd : Fd.t) : t =
    object
      inherit t_from_output ~bytes:buf.bytes ()

      method private output_underlying bs i len0 =
        let i = ref i in
        let len = ref len0 in
        while !len > 0 do
          match MIO.Unix.write fd bs !i !len with
          | 0 -> failwith "write failed"
          | n ->
            i := !i + n;
            len := !len - n
        done

      method private close_underlying () =
        if close_noerr then (
          try MIO.Unix.close fd with _ -> ()
        ) else
          MIO.Unix.close fd
    end
end

(** Buffered input *)
module Input = struct
  include Iostream.In_buf

  class of_unix_fd ?(close_noerr = false) ~(buf : Slice.t) (fd : Fd.t) : t =
    let eof = ref false in
    object
      inherit Iostream.In_buf.t_from_refill ~bytes:buf.bytes ()

      method private refill (slice : Slice.t) =
        if not !eof then (
          slice.off <- 0;
          let continue = ref true in
          while !continue do
            match MIO.Unix.read fd slice.bytes 0 (Bytes.length slice.bytes) with
            | n ->
              slice.len <- n;
              continue := false
          done;
          (* Printf.eprintf "read returned %d B\n%!" !n; *)
          if slice.len = 0 then eof := true
        )

      method close () =
        if close_noerr then (
          try MIO.Unix.close fd with _ -> ()
        ) else
          MIO.Unix.close fd
    end
end
