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
  (ocaml (>= 4.13))
  dune
  (moonpool (>= 0.6))
  (containers
   (>= 3.4))
  (iter
   (>= 1.0))
  logs
  base64
  uuidm
  ptime
  atomic
  hmap
  thread-local-storage
  (yojson (>= 1.6))
  camlzip
  (mtime
   (>= 2.0))
  ppx_deriving
  (ppxlib (>= 0.25.0))
  (trace (>= 0.7))
  (qcheck-core (and (>= 0.18) :with-test))
  (trace-tef :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat (and (= 0.24.1) :with-dev-setup))
  (odoc :with-doc))
 (tags
  (moonpool multicore thread logs logging)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
