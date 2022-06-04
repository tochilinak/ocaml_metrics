(lang dune 2.8)

(cram enable)

(generate_opam_files true)

(source
 (github tochilinak/ocaml_metrics))

(name ocaml_metrics)

(package
 (name ocaml_metrics)
 (depends yojson ppxlib sexplib ppx_inline_test base stdio ocamlgraph))
