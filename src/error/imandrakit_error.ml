(** Universal errors.

    This library provides an error type that can be used in many situations,
    can contain metadata (locations, etc.), has context (a bit like
    explicitly-managed stack traces), and can be serialized.
*)

module Data = Data
module Error = Error
module Error_core = Error_core
module Kind = Kind
