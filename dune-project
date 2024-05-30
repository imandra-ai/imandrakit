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
  (ocaml
   (>= 4.13))
  dune
  (containers
   (>= 3.4))
  (iter
   (>= 1.0))
  base64
  uuidm
  ptime
  atomic
  hmap
  (yojson
   (>= 1.6))
  (mtime
   (>= 2.0))
  ppx_deriving
  (ppxlib
   (>= 0.25.0))
  (trace
   (>= 0.7))
  (qcheck-core
   (and
    (>= 0.18)
    :with-test))
  (trace-tef :with-test)
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    (= 0.24.1)
    :with-dev-setup))
  (hex
   (and
    :with-test
    (>= 1.5)))
  (odoc :with-doc))
 (depopts camlzip)
 (tags
  (moonpool multicore thread logs logging)))

(package
 (name imandrakit-io)
 (synopsis "IO utils for Imandra")
 (depends
  (ocaml
   (>= 4.13))
  (imandrakit
   (= :version))
  (moonpool
   (>= 0.6))
  (trace-tef :with-test)
  (odoc :with-doc)))

(package
 (name imandrakit-log)
 (synopsis "Logging utils for Imandra")
 (depends
  (ocaml
   (>= 4.13))
  (imandrakit
   (= :version))
  hmap
  dune
  (moonpool
   (>= 0.6))
  logs
  thread-local-storage
  (trace-tef :with-test)
  (odoc :with-doc)))

(package
 (name imandrakit-thread)
 (synopsis "Threading and concurrency for Imandra")
 (depends
  (ocaml
   (>= 5.0))
  (imandrakit
   (= :version))
  logs
  hmap
  dune
  (moonpool
   (>= 0.6))
  thread-local-storage
  (trace-tef :with-test)
  (odoc :with-doc)))

(package
 (name twine-utils)
 (synopsis "toolkit program for twine")
 (depends
  imandrakit
  imandrakit-log
  yojson
  hex
  ppx_deriving
  logs
  trace
  trace-tef
  containers
  ppx_subliner))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
