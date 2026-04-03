(lang dune 3.20)

(name origami-app)

(generate_opam_files true)

(package
 (name origami-app)
 (synopsis "Paper size calculator for origami boxes")
 (depends ocaml brr brr-lwd lwd vg gg js_of_ocaml))
