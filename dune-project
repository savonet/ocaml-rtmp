(lang dune 2.0)
(version 0.1.0)
(name rtmp)
(source (github savonet/ocaml-rtmp))
(license LGPL-2.1)
(authors "Samuel Mimram <samuel.mimram@ens-lyon.org>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name rtmp)
 (synopsis "Native OCaml implementation of the RTMP protocol")
 (depends
  (dune (>= 2.0))))
