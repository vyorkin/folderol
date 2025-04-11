(lang dune 3.16)

(using menhir 2.0)
(name folderol)
(generate_opam_files true)

(source
 (github vyorkin/folderol))
(authors "Vasiliy Yorkin")
(maintainers "Vasiliy Yorkin <vasiliy.yorkin@gmail.com>")
(license MIT)

(package
 (name folderol)
 (synopsis "My little theorem prover")
 (description
  "Toy theorem prover based on the Designing a Theorem Prover paper by Lawrence C. Paulson.")
 (depends
  (ocaml
   (>= 5.3))
  (alcotest :with-test)
  (core :build)
  ppx_let
  (ppx_deriving
   (>= 6.0))
  (alcotest
   (>= 1.9))
  (menhir
   (and
    :build
    (= 20240715)))
  (menhirLib
   (and
    :build
    (= 20240715)))))
