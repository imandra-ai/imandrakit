(** FNV hashing.

 A reasonably fast and simple, non cryptographic, hash *)

type ctx
(** Mutable context, holding an int64 *)

val int64 : (ctx, int64) Imandrakit_mhash.hash_algo
(** FNV hashing, returning an int64. *)

val int : (ctx, int) Imandrakit_mhash.hash_algo
(** FNV hashing, returning an int. The int is obtained from an [int64]
    and then truncated and made nonnegative. *)

val hash_int : 'a Imandrakit_mhash.hasher -> 'a -> int
val hash_int64 : 'a Imandrakit_mhash.hasher -> 'a -> int64
