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
  (ocaml (>= 4.03))
  (dune (>= 1.11))
  integers
  (angstrom (>= 0.10.0))
  faraday
  (ounit (and :with-test (>= 2.0))))
 (synopsis "Library for describing Protobuf messages")
 (description "Library for describing Protobuf messages"))

(package
 (name pb-plugin)
 (depends
  (ocaml (>= 4.03))
  pb
  (dune (>= 1.11))
  integers
  batteries
  (angstrom (>= 0.10.0))
  faraday
  (ounit (and :with-test (>= 2.0))))
 (synopsis "Plugin for generating pb protobuf message descriptions")
 (description "Plugin for generating pb protobuf message descriptions"))
