open Chalumeau

let make_ml = "make.ml"

let initial_cwd = Unix.getcwd ()

let chalumeau_cma =
  let path = Filename.dirname Sys.executable_name in
  let path =
    if Filename.is_relative path then
      initial_cwd /^ path
    else
      path
  in
  match
    List.find exists [
      (* Default is next to the executable. *)
      path /^ "chalumeau.cma";

      (* Some systems could prefer to put it in a lib directory
         next to the executable. *)
      path /^ "lib" /^ "chalumeau.cma";

      (* When chalumeau has not been installed and is run directly from
         where it has been compiled, we look in the compilation directory. *)
      path /^ "bin" /^ "src" /^ "chalumeau.cma";
    ]
  with
    | exception Not_found ->
        print_endline
          "Could not find Chalumeau library next to Chalumeau executable.";
        exit 1
    | s ->
        s

let rec find_make_ml cwd =
  if Sys.file_exists (cwd /^ make_ml) then
    Some cwd
  else
    match parent cwd with
      | None ->
          None
      | Some parent ->
          find_make_ml parent

let () =
  match find_make_ml initial_cwd with
    | None ->
        print_endline "Could not find make.ml here nor in a parent directory."
    | Some path ->
        if path <> initial_cwd then
          print_endline ("chalumeau: Entering directory `" ^ path ^ "'");
        Unix.chdir path;

        run "ocaml" [
          "-I"; Filename.dirname chalumeau_cma;
          "unix.cma"; "chalumeau.cma"; make_ml;
        ] |> check;

        if path <> initial_cwd then
          print_endline ("chalumeau: Leaving directory `" ^ path ^ "'")
