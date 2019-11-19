(lang dune 1.11)
(name pb)

(generate_opam_files true)

(license MIT)

(maintainers "Jeremy Yallop <yallop@gmail.com>")
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
  (ounit :with-test))
 (synopsis "Library for describing Protobuf messages")
 (description "Library for describing Protobuf messages"))
