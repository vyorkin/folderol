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
 (description "Toy theorem prover based on the Designing a Theorem Prover paper by Lawrence C. Paulson.")
 (depends ocaml dune))
