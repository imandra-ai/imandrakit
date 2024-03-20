(lang dune 3.0)

(name imandrakit)
(version 0.1)
(generate_opam_files true)
(source
 (github imandra-ai/imandrakit))
(authors "Simon Cruanes")
(maintainers "Simon Cruanes")
(license MIT)

;(documentation https://url/to/documentation)

(package
 (name imandrakit)
 (synopsis "Core utils library for Imandra")
 (depends
  (ocaml (>= 5.0))
  dune
  (moonpool (>= 0.6))
  (containers
   (>= 3.4))
  (iter
   (>= 1.0))
  cryptokit
  logs
  base64
  uuidm
  ptime
  atomic
  hmap
  thread-local-storage
  camlzip
  (mtime
   (>= 2.0))
  (zarith
   (>= 1.12))
  ppx_deriving
  (trace :with-test)
  (trace-tef :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat (and (= 0.24.1) :with-dev-setup))
  (odoc :with-doc))
 (depopts
   (trace (>= 0.6)))
 (tags
  (moonpool multicore thread logs logging)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
