(lang dune 3.8)

(name indexedmap)

(generate_opam_files true)

(source
 (github mizlan/indexedmap))

(authors "Michael Lan")

(maintainers "Michael Lan")

(license MIT)

(documentation https://github.com/mizlan/indexedmap)

(package
 (name indexedmap)
 (synopsis "Purely-functional order-statistic red-black trees offering great asymptotic performance")
 (description "Purely-functional order-statistic red-black trees offering great asymptotic performance")
 (depends ocaml dune (alcotest :with-test))
 (tags
  ("data structure" tree)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
