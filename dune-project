(lang dune 1.11)
(name pb)

(generate_opam_files true)

(implicit_transitive_deps false)

(license MIT)

(maintainers
  "Jeremy Yallop <yallop@gmail.com>"
  "Rudi Grinberg <me@rgrinberg.com>")
(authors "Jeremy Yallop <yallop@gmail.com>")
(source (github yallop/ocaml-pb))

(package
 (name pb)
 (depends
  ocaml
  (dune (>= 1.11))
  integers
  base-bytes
  result
  (angstrom (>= 0.10.0))
  faraday
  (ounit (and :with-test (>= 2.0))))
 (synopsis "Library for describing Protobuf messages")
 (description "Library for describing Protobuf messages"))
