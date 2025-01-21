(lang dune 3.16)

(name acorn)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "chameko")

(maintainers "chameko")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name acorn)
 (synopsis "A markup format for kakoune")
 (description "A markup format for kakoune")
 (depends ocaml ppx_expect ppx_deriving)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
