(* We compile Chalumeau using Chalumeau.
   Because we did not compile the library yet we have to use it like this. *)
#directory "src"
#load "unix.cma"
#use "chalumeau.ml"

let () =
  (* Library. *)
  simple_library "src/chalumeau";

  (* Program. *)
  best (program "chalumeau" (directory "src") "src/main")
