(see: https://github.com/imandra-ai/imandrakit/discussions/2)

## Overview

We need a relatively general purpose format to serialize complex OCaml values and is better behaved/more stable than Marshall. This new format I envision is a mix of [fleece](https://github.com/couchbase/fleece) and [UBJSON](https://ubjson.org/). From fleece I'd like to take the 0-copy, pointer-heavy format that enables efficient sharing (like the "pack" in "serpack"). From UBJSON I'd like to take the extreme simplicity (a single mnemonic byte is used to prefix every value) and semi-readability. I also want to extend the data model from being JSON-like (both fleece and UBJSON) into a more static, more complex world; namely, I'd like a direct representation for constructors, records, pointers (kind of like fleece), _maybe_ relative pointers (exactly like fleece), and foreign keys (to enable GC in key-value storage, where values contain other keys). Lastly, it's important to be able to read the format without schema (for debug tools, GC, etc.) so it must be somewhat self-describing.

## Name

Suggestions welcome!

## General principles

All parsing is done directly on a `bytes` value (that we call "the slice"), without an intermediate copy. A value is referred to by an index in the `bytes`. Pointers are int-typed values that point to other parts of the slice[^1] and enable sharing. Every single value starts with a single mnemonic byte (e.g. `S` for strings, `i`, `I`, `l`, `L`, for integers of various sizes, `d`, `D` for floats, etc.).

For composite types, we can be a bit more inventive. Since the primary use case is to encode values from a static language (OCaml), a general purpose dictionary is less useful. We can instead have custom representations for _records_ and _constructors_ of sum types. null can be used for `unit`, arrays for tuples, etc. Because of static typing we know that many records/sum types will share their representation, so we can factor that out into a _descriptor_ and then use only a pointer to this descriptor. 

Relative pointers (maybe?) are offsets from the current value to a value coming earlier in the slice (ie if at position `1000` we have a relative pointer of `200`, it points to the value at `800`; so it behaves like a pointer with value `800` but might be more compact).

[^1]: pointers should always point to values coming before them in the slice, since we're going to build the slice from left to right using an append-only `Buffer.t`.

## Format

### Scalar values

All binary values are in little-endian.

- `"i" <u8>` is a small nonnegative int (2B) 
- `"I" <i16>` is a small int (3B)
- `"l" <i32>` is a small int (5B)
- `"L" <i64>` is an int (9B)
- `"T"` is true (1B)
- `"F"` is false (1B)
- `"N"` is null/nil (1B)
- `"d" <f32>` is a float (5B) (mnemo: decimal, since "F" is taken already)
- `"D" <f64>` is a float (9B)
- `"s" <n:u8> <n bytes>` is a small string with < 256 bytes (1 + 1 + n B)
- `"S" <n:int value> <n bytes>"` is a long string  (1 + sizeof(n) + n B) (with a variadic size value)
- `"b" <n:u8> <n bytes>` is a small blob with < 256 bytes (1 + 1 + n B)
- `"B" <n:int value> <n bytes>"` is a long blob (1 + sizeof(n) + n B)

### Pointers

- `"P" <n:int value>` is a pointer to the value at offset `n` in the slice (1 + sizeof(n)) bytes
- (optional) `"p" <n:u16>` is a relative pointer to a value `n` bytes before the current one (3B)

### Structures

- `"{" ( <string> <value> )* "}"` is a dictionary (2B+data), similar to UBJSON. No separator.
- `"[" <value>* "]"` is an array (2B+data), similar to UBJSON. No separator.
- `"R" <p: int value> <vs:value>+` is a record with fields `vs`. The number of fields can be found in the record descriptor pointed to by `p`. (1 + sizeof(p) + sum sizeof(vs)). The pointer `p` is absolute and should probably be small (if we emit pointers early on their offsets will be small and thus pointers will be more compact).
  * descriptor: a dictionary `d` containing potentially multiple things (e.g. a comment, the hash of the type + deps, etc.) and a `d.fs` (fields) key with an array of strings inside. The number of fields for values of this record type is the length of `d.fs`, the name of the `n`-th field is given by `d.fs[n]`. For example the descriptor for the `Complex.t` type would be `{ fs: ["re", "im"] }`.
  * For example, the value `{re=1.0; im=2.7} : Complex.t}` would be of the shape `"R" <pointer to the descriptor> "f" <1.0> "f" <2.7>`.
- `"C" <p:int value> <n:u8> <args:value>*` is a constructor for some sum type (1 + sizeof(p) + 1 + sum sizeof(args)). The sum type's descriptor is at `p` (an absolute pointer). The integer `n` is the constructor's index. `args` is a list of arguments whose length is found in the descriptor.
  * descriptor: a dictionary `d` containing potentially multiple things. It must contain a field `d.cs` (constructors) that's an array of dictionaries. Each constructor's dictionary must contain at least a key `ar:int` such that `d.cs[i].ar` is the arity of the `i`-th constructor of this type. The constructor's dictionary can optionally have a key `fs: array<string>` to describe inline records.
- `"K" <v:value>` tags a value `v` with the property of being a _key_ (or foreign key). This is useful to indicate that this value is used somewhere else as a key in some key/value storage and enables schema-less GC[^2].

We do not, for now, seek to imitate the typed arrays of UBJSON, for the sake of simplicity. Other data types could be useful but I think this is a good start. When hand-writing (de)serializers, or in the ppx, we can query the immediate value at offset `p` by asking for an explicit type of data, fail if it's the wrong type, etc. For a sum type we would have both `get_sum_cstor: slice -> offset -> int` and then `get_sum_arg:slice -> offset -> int -> offset`. For a record we get `get_record_arg : slice -> offset -> offset`.

**NOTE**: arguments to constructors/records should be scalar values or pointers, because we have to traverse them to get to the next one. There's a bit of speed loss compared to fleece because fleece's values are exactly 2B or 4B wide each. Maybe our interface should be more like `get_record : slice -> offset -> int * offset Iter.t` (and `get_record_descriptor: slice -> offset -> offset`). Similarly `get_sum_cstor: slice -> offset -> (* cstor *) int * (* arity *) int * offset Iter.t`?

[^2]: given a list of initial roots, we can traverse the K/V store and find which other keys does a given alive K/V pair point to. Then we remove the dead keys that are not reachable. This can also be used to compute bundles of K/V pairs: in a single download, get all K/V pairs transitively used in the representation of the initial key, so that no further query is needed.
