(lang dune 3.8)
(using melange 0.1)
(name decco)

(generate_opam_files true)

(source (github rolandpeelen/decco))
(homepage "https://github.com/rolandpeelen/decco")
(bug_reports "https://github.com/rolandpeelen/decco")
(authors "Roland Peelen <rolandpeelen@gmail.com>")
(license "")
(documentation "") 

(package
 (name decco)
 (synopsis "Melange Pancake")
 (description "Bucklescript PPX which generates JSON (de)serializers for user-defined types")
 (allow_empty)
 (depends
  (ocaml (= 5.1.1))
  (dune (>= 3.17))

  (melange (>= 4.0 ))
  
  (reason (>= 3.15))
  (ppxlib (>= 0.31))

  (ocaml-lsp-server :dev)
  (melange-jest :with-test)
  (odoc :with-doc)))
