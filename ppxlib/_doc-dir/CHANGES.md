0.32.1 (2024-04-23)
-------------------

- Add support for OCaml 5.2

- Insert errors from caught located exceptions in place of the code that
  should have been generated by context-free rules. (#472, @NathanReb)

0.32.0 (2024-02-05)
-------------------

- Add an optional `embed_errors` argument to `Context_free.map_top_down` that
  controls how to deal with exceptions thrown by context-free rules.
  (#468, @NathanReb)

- Fix `Longident.parse` so it properly handles unparenthesized dotted operators
  such as `+.` or `*.`. (#111, @rgrinberg, @NathanReb)

- raising an exception does no longer cancel the whole context free phase(#453, @burnleydev1)

- Sort embedded errors that are appended to the AST by location so the compiler
  reports the one closer to the beginning of the file first. (#463, @NathanReb)

- Update `Attribute.get` to ignore `loc_ghost`. (#460, @ceastlund)

- Add API to manipulate attributes that are used as flags (#408, @dianaoigo)

- Update changelog to use ISO 8061 date format: YYYY-MM-DD. (#445, @ceastlund)

- Replace `Caml` with `Stdlib`. (#427, @ceastlund)

- When a transformation raises, the last valid AST is used as input to the upcoming
  transformations. All such errors are collected and appended as
  extension nodes to the final AST (#447, @burnleydev1)

- Fix a small mistake in the man pages: Embededding errors is done by default with
  `-as-pp`, not with `-dump-ast` (#464, @pitag-ha)

- Set appropriate binary mode when writing to `stdout` especially for Windows
  compatibility. (#466, @jonahbeckford)

0.31.0 (2023-09-21)
-------------------

- Fix support for OCaml 5.1: migrated code preserves generative
  functor warnings, without creating more. Locations are better
  preserved. (#432, @pitag-ha, @panglesd)

- Driver: Add `-unused-code-warnings` command-line flag. (#444, @ceastlund)

- Add `?warning` flag to `Deriving.Generator.make`. (#440, @jacksonzou123 via @ceastlund)

- Restore the "path_arg" functionality in the V3 API (#431, @ELLIOTTCABLE)

- Expose migration/copying/etc. functions for all AST types needed by `Pprintast` (#454, @antalsz)

- Preserve quoted attributes on antiquotes in metaquot (#441, @ncik-roberts)

- Attribute namespaces: Fix semantics of reserving multi-component namespaces (#443, @ncik-roberts)

0.30.0 (2023-06-20)
-------------------

- Adopt the OCaml Code of Conduct on the repo (#426, @pitag-ha)

- Clean up misleading attribute hints when declared for proper context. (#425, @ceastlund)

- Ast_pattern now has ebool, pbool helper, and a new map.(#402, @burnleydev1)

- multiple errors are now reported in `metaquot`. (#397, @burnleydev1)

- Add `Attribute.declare_with_attr_loc` (#396, @dvulakh)

- Add "ns" and "res" as reserved namespaces(#388, @davesnx)

- Make quoter `let` binding non-recursive (#401, @sim642)

- Fix failure of 'lift_map_with_context' in traverse by compile-time
  evaluation of 'fst' and 'snd' (#390, @smuenzel)

- Driver: Bias the mapping from magic to version towards the current version,
  as it is usually the common case and it helps when magic numbers are
  ambiguous (such as on development versions) (#409, @shym)

- Remove unnecessary test dependencies towards base and stdio (#421, @kit-ty-kate)

- Update description to reflect that `ppxlib` contains more than a library
  (#422, @pitag-ha)

- Add support for OCaml 5.1, excluding OCaml `5.1.0~alpha1` (#428, @shym, @Octachron , @pitag-ha, @panglesd)
- Driver: Fix `-locations-check` option for coercions with ground  (#428, @Octachron)

0.29.1 (2023-02-14)
------------------

- Allow users to vendor `ppxlib` as-is, as well as `ppx_sexp_conv` in the same project (#386, @kit-ty-kate)

0.29.0 (2023-02-06)
------------------

- Remove `File_path` exports. (#381, @ceastlund)

- Add `Ppxlib.Expansion_helpers` with name mangling utilities from ppx_deriving (#370, @sim642)

0.28.0 (2022-10-05)
-------------------

- Make `esequence` right-associative. (#366, @ceastlund)

- Deprecate unused attributes in `Deriving.Generator` (#368, @sim642)

- Remove a pattern match on mutable state in a function argument. (#362, @ceastlund)

- Add code-path manipulation attributes. (#352, @ceastlund)

- Update context-free rules to collect expansion errors generated by ppxlib and
  propagate them to top level without failing. (#358 and #361, @ceastlund)

- Add driver benchmarks (#376, @gridbugs)

0.27.0 (2022-06-14)
-------------------

- Update expansion context to leave out value name when multiple are
  defined at once. (#351, @ceastlund)

- Add support for OCaml 5.0 (#348, @pitag-ha)

- Add `Code_path.enclosing_value` (#349, @ceastlund)

- Add `Code_path.enclosing_module` (#346, @ceastlund)

- Expand code generated by `~enclose_intf` and `~enclose_impl` (#345, @ceastlund)

- Add type annotations to code generated by metaquot (#344, @ceastlund)

- Fix typo in description field of dune-project (#343, @ceastlund)

- Fix Ast_pattern.many (#333, @nojb)

- Fix quoter and optimize identifier quoting (#327, @sim642)

- Driver, when run with `--check`: Allow `toplevel_printer` attributes (#340, @pitag-ha)

- Documentation: Add a section on reporting errors by embedding extension nodes
  in the AST (#318, @panglesd)

- Driver: In the case of ppxlib internal errors, embed those errors instead of
  raising to return a meaningful AST (#329, @panglesd)

- API: For each function that could raise a located error, add a function that
  return a `result` instead (#329, @panglesd)

0.26.0 (2022-03-21)
-------------------

- Bump ppxlib's AST to 4.14/5.00 (#320, @pitag-ha)

0.25.1 (2022-06-17)
-------------------

- Add support for OCaml 5.0 (#355, @pitag-ha)

0.25.0 (2022-03-03)
-------------------

- Added `error_extensionf` function to the `Location` module (#316, @panglesd)

- Ast patterns: add `drop` and `as` patterns (#313 by @Kakadu, review by @pitag-ha)

- Fixed a bug resulting in disscarded rewriters in the presence of
  instrumentations, as well as a wrong order of rewriting (#296, @panglesd)

- Driver: Append the last valid AST to the error in case of located exception
  when embedding errors (#315, @panglesd)

0.24.0 (2021-12-08)
-------------------

- Add support for OCaml 4.14 (#304, @kit-ty-kate)

- Expand nodes before applying derivers or other inline attributes based
  transformation, allowing better interactions between extensions and
  derivers (#279, #297, @NathanReb)

- Add support for registering ppx_import as a pseudo context-free rule (#271, @NathanReb)

- Add `input_name` to the `Expansion_context.Extension` and `Expansion_context.Deriver` modules (#284, @tatchi)

- Improve `gen_symbol` to strip previous unique suffix before adding a new one (#285, @ceastlund)

- Improve `name_type_params_in_td` to use prefixes `a`, `b`, ... instead of `v_x`. (#285, @ceastlund)

- Fix a bug in `type_is_recursive` and `really_recursive` where they would
  consider a type declaration recursive if the type appeared inside an attribute
  payload (#299, @NathanReb)


0.23.0 (2021-08-31)
-------------------

- Drop `Parser` from the API (#263, @pitag-ha)

- `Location`: add `set_filename` and `Error.get_location` (#247, @pitag-ha)

- Drop dependency on OMP2 (#187, @pitag-ha)

- Make OMP1 a conflict (#255, @kit-ty-kate)

- Drop `Syntaxerr` from the public API. Doesn't affect any user in the
  [ppx universe](https://github.com/ocaml-ppx/ppx_universe) (#244, @pitag-ha)

- Add a lower-bound constraint for Sexplib0 (#240, @pitag-ha)

- Fix bug due to which unwanted public binaries got installed when installing
  ppxlib (#223, @pitag-ha)

- Add `Keyword.is_keyword` to check if a string is an OCaml keyword
  (#227, @pitag-ha)

- Remove `Lexer.keyword_table`: use `Keyword.is_keyword` instead
  (#227, @pitag-ha)

- Remove `Lexer` from the API: it was the same as the compiler-libs
  `Lexer` (#228, @pitag-ha)

- Remove the modules `Ast_magic`, `Compiler_version`, `Js`, `Find_version`,
  `Convert`, `Extra_warnings`, `Location_error`, `Select_ast` and
  `Import_for_core` from the API: they are meant for internal use and
  aren't used by any current downstream user in the
  [ppx universe](https://github.com/ocaml-ppx/ppx_universe) (#230, @pitag-ha)

- Remove compiler specific helper functions from `Location`. They aren't used
  by any current downstream user in the
  [ppx universe](https://github.com/ocaml-ppx/ppx_universe) (#238, @pitag-ha)

- Allow "%a" when using Location.Error.createf (#239, @mlasson)

- Fix in `Location`: make `raise_errorf` exception equivalent to exception
  `Error` (#242, @pitag-ha)

- Fix in `Pprintast`: correctly pretty print local type substitutions, e.g.
  type t := ... (#261, @matthewelse)

- Add `Ast_pattern.esequence`, for matching on any number of sequenced
  expressions e.g. `do_a (); do_b (); ...`. (#264, @matthewelse)

- Expose a part of `Ast_io` in order to allow reading AST values from binary
 files (#270, @arozovyk)

0.22.2 (2021-06-23)
-------------------

- Make ppxlib compatible with 4.13 compiler (#260, @kit-ty-kate)

0.22.1 (2021-06-10)
-------------------

- Fix location in parse error reporting (#257, @pitag-ha)

0.21.1 (2021-06-09)
-------------------

- Fix location in parse error reporting (#256, @pitag-ha)

0.22.0 (2021-02-04)
-------------------

- Bump ppxlib's AST to 4.12 (#193, @NathanReb)

0.21.0 (2021-01-22)
-------------------

- Fix ppxlib.traverse declaration and make it a deriver and not a rewriter
  (#213, @NathanReb)
- Driver (important for bucklescript): handling binary AST's, accept any
  supported version as input; preserve that version (#205, @pitag-ha)

- `-as-ppx`: take into account the `-loc-filename` argument (#197, @pitag-ha)

- Add input name to expansion context (#202, @pitag-ha)

- Add Driver.V2: give access to expansion context in whole file transformation
  callbacks of `register_transformation` (#202, @pitag-ha)

- Driver: take `-cookie` argument into account, also when the input is a
  binary AST (@pitag-ha, #209)

- `run_as_ppx_rewriter`: take into account the arguments
  `-loc-filename`, `apply` and `dont-apply` (#205, @pitag-ha)

- Location.Error: add functions `raise` and `update_loc`
  (#205, @pitag-ha)

0.20.0 (2020-11-16)
-------------------

- Expose `Ppxlib.Driver.map_signature` (#194, @kit-ty-kate)

0.19.0 (2020-10-23)
-------------------

- Make ppxlib compatible with 4.12 compiler (#191, @kit-ty-kate)

0.18.0 (2020-10-06)
-------------------

- Bump ppxlib's AST to 4.11 (#180, @NathanReb)

0.17.0 (2020-09-17)
-------------------

- Add accessors for `code_path` and `tool_name` to `Expansion_context.Base`
  (#173, @jberdine)
- Add `cases` methods to traversal classes in `Ast_traverse` (#183, @pitag-ha)

0.16.0 (2020-08-18)
-------------------

- `Driver.register_transformation`: add optional parameter `~instrument`
  (#161, @pitag-ha)
- Add missing `Location.init` (#165, @pitag-ha)
- Upgrade to ocaml-migrate-parsetree.2.0.0 (#164, @ceastlund)

0.15.0 (2020-08-04)
-------------------

- Remove `base` and `stdio` dependencies (#151, @ceastlund)

- Update README and opam description (#155, @jeremiedimino)

- Fix `Driver.partition_transformation` (#156, @NathanReb)

- Implement name mangling for `ppxlib_traverse` (#159, @ceastlund)

0.14.0 (2020-07-08)
-------------------

- Bump ppxlib's AST to 4.10 (#130, @NathanReb)

- Remove omp_config from `Expansion_context` and replace it with `tool_name`
  (#149, @NathanReb)

- Change undocumented `Ppxlib.Driver.map_structure` to return a ppxlib's
  `structure` instead of a `Migrate_parsetree.Driver.some_structure`.
  (#153, @NathanReb)

0.13.0 (2020-04-15)
-------------------

- Add 'metaquot.' prefix to disambiguate metaquote extensions (#121,
  @ceastlund)

- Bump dune language to 1.11 since the cinaps extension requires at
  least Dune 1.11 (#126, @diml)

0.12.0 (2020-01-07)
-------------------

- Support for OCaml 4.10 (#109, @xclerc)

0.11.0 (2020-01-07)
-------------------

- Invariant check on locations (#107, @trefis)

0.10.0 (2019-11-21)
-------------------

- Do not produce a suprious empty correction when deriving_inline
  expands into an extension that undergoes further expansion (#86,
  @aalekseyev)

- Add `Ppxlib.Quoter`. This module allows to generate hygienic code fragments in
  the spirit of ppx_deriving. (#92, @rgrinberg)

- Allow for registering derivers on module type declarations. (#94, fix #83,
  @rgrinberg)

- Fix parsing long idenitifiers. (#98, @NathanReb)

0.9.0
-----

- Bump AST to 4.08 (#80, @xclerc)

0.8.1
-----

### Fixed

- Report errors according to the value of `OCAML_ERROR_STYLE` and
  `OCAML_COLOR` in the standalone driver (#83, @NathanReb)

0.6.0
-----

- Set `Location.input_name` to the original filename when reading a
  binary AST (#.., @diml)

0.5.0
-----

- Add an `(** @inline *)` to the include generated when silencing
  warning 32 (#58, @trefis)

- Add `Ppxlib.mk_named_sig` and `Ppxlib.is_polymorphic_variant` (#57,
  @trefis)

0.4.0
-----

- Do not report errors about dropped or uninterpreted attributes
  starting with `_` (#46, fix #40, @diml)

- Fix he `special_function` rule for dotted operators and allow
  `Longident.parse` to parse dotted operators (#44, @Octachron)

- Port to `dune` and remove use of bash (#45, @rgrinberg)

- Ignore all attribites starting with `_` (#46, @diml)

- Reserve the `reason` and `refmt` namespaces (#46, @diml)

- Reserve the `metaocaml` namespace (#50, @rgrinberg)

- Fix attribute extraction for Otag/Rtag (#51, @xclerc)

- Do not relocate files unless `-loc-filename` is passed (#55, @hhugo)

- Preserve the filename in the output (#56, @hhugo)

0.3.1
-----

- Add `Attribute.declare_with_name_loc` (#33, @diml)

- Let the tool name pass thought when used as a -ppx (#41, @diml)

- Update the AST to 4.06 (#8, @xclerc)

0.3.0
-----

- Update the AST to 4.06 (#8, @xclerc)

- Deprecate old references to type_conv in argument and rewriter names
  and add new ones mentioning deriving instead (#7, #9 @xclerc)

- Fix compatibility with `-safe-string` (#10, @hhugo)

- Restore tests (#11, @xclerc)

- Allow to set the suffix of corrected files (#15, @diml)

- Restore compatibility with OCaml 4.04.x (#16, @xclerc)

0.2.0
-----

- Make sure to import command line arguments registered with
  ocaml-migrate-parsetree (#5, @diml)

- Fix an issue where cookies set from the command line sometimes
  disappeared (#6, @diml)

0.1.0
-----

Initial release.