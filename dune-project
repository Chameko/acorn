(lang dune 3.17)

(name acorn)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "chameko")

(maintainers "chameko")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name acorn)
 (synopsis "A markup format for kakoune")
 (description "A markup format for kakoune")
 (depends
  ocaml
  ppx_expect
  ppx_deriving
  ppx_sexp_conv
  (sexplib
   (>= 0.17.0))
  (fmt
   (>= 0.9.0))
  (lwt
   (>= 5.5.0))
  (stdio
   (>= 0.17.0))
  (lwt_ppx
   (>= 5.8.0))
  (logs
   (>= 0.7.0))
  (cmdliner
   (>= 1.3.0)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
