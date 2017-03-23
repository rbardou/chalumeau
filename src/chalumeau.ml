let a = true
let c = true
let p = true
let s = true
let f = true

let rec list_filter_map ?(acc = []) f l =
  match l with
    | [] ->
        List.rev acc
    | hd :: tl ->
        let acc =
          match f hd with
            | None -> acc
            | Some x -> x :: acc
        in
        list_filter_map ~acc f tl

let quote_shell path =
  let len = String.length path in
  let buf = Buffer.create (len * 2) in

  (* We find the longest chunks without quotes.
     Chunks are copied between quotes if they contain characters such as spaces.
     Quotes are escaped. *)
  let chunk_start = ref 0 in
  let chunk_needs_quote = ref false in

  let end_chunk next =
    let cnq = !chunk_needs_quote in
    let start = !chunk_start in
    if cnq then Buffer.add_char buf '\'';
    Buffer.add_substring buf path start (next - start);
    if cnq then Buffer.add_char buf '\'';
    chunk_start := next
  in

  for i = 0 to len - 1 do
    match path.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '/' | '.' ->
          ()
      | '\'' ->
          end_chunk i;
          incr chunk_start;
          Buffer.add_string buf "\\'"
      | _ ->
          chunk_needs_quote := true
  done;

  end_chunk len;
  Buffer.contents buf

let split_string sep str =
  let len = String.length str in
  let start = ref 0 in
  let acc = ref [] in

  let end_block pos =
    acc := String.sub str !start (pos - !start) :: !acc;
    start := pos + 1
  in

  for i = 0 to len - 1 do
    if str.[i] = sep then
      end_block i
  done;

  end_block len;

  List.rev !acc

let split_lines str =
  let len = String.length str in
  let start = ref 0 in
  let acc = ref [] in

  let end_block pos =
    (* Only return non-empty lines. *)
    if pos <> !start then
      acc := String.sub str !start (pos - !start) :: !acc;
    start := pos + 1
  in

  for i = 0 to len - 1 do
    if str.[i] = '\r' || str.[i] = '\n' then
      end_block i
  done;

  end_block len;

  List.rev !acc

let (//) o x =
  match o with
    | None -> x
    | Some x -> x

let (/^) = Filename.concat

let (//^) o x =
  match o with
    | None -> x
    | Some y -> y /^ x

let chop_extension filename =
  match Filename.chop_extension filename with
    | exception Invalid_argument _ -> filename
    | s -> s

let exists filename =
  (* TODO: Sys.file_exists returns false if [filename] is a symbolic link
     to a file which does not exist, which may not be what we want. *)
  match Sys.file_exists filename with
    | exception _ -> false
    | b -> b

let which filename =
  let path =
    match Sys.getenv "PATH" with
      | exception Not_found ->
          []
      | path ->
          (* TODO: on Windows, is it not ';' instead of ':'? *)
          split_string ':' path
  in
  match List.find exists (List.map (fun path -> path /^ filename) path) with
    | exception Not_found ->
        None
    | found ->
        Some found

let parent filename =
  let parent = Filename.dirname filename in
  if String.length parent >= String.length filename then
    None
  else
    Some parent

let absolute filename =
  if Filename.is_relative filename then
    Sys.getcwd () /^ filename
  else
    filename

let handle_unix_error f =
  Unix.handle_unix_error f ()

let ln ?(s = false) ?(f = false) target name =
  handle_unix_error @@ fun () ->
  if f && exists name then
    Sys.remove name;
  if s then
    Unix.symlink target name
  else
    Unix.link target name

let link target name =
  let target = absolute target in
  if
    match Unix.readlink name with
      | exception Unix.Unix_error ((Unix.ENOENT | Unix.EINVAL), _, _) ->
          true
      | existing_target ->
          existing_target <> target
  then (
    print_endline ("ln -s -f " ^ quote_shell target ^ " " ^ quote_shell name);
    ln ~s ~f target name
  )

let rec mkdir ?(p = false) name =
  if p then (
    match parent name with
      | None ->
          ()
      | Some parent ->
          mkdir ~p parent
  );
  if not p || not (exists name) then
    Unix.mkdir name 0o751

let make_directory name =
  if not (exists name) then (
    print_endline ("mkdir -p " ^ quote_shell name);
    mkdir ~p name
  )

let ls dir =
  List.map ((/^) dir) (Array.to_list (Sys.readdir dir))

let run program arguments =
  let argv = program :: arguments in
  let pid =
    Unix.create_process program (Array.of_list argv)
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  status

let run_in program arguments =
  let argv = program :: arguments in

  (* Create process for reading.
     Not using [Unix.open_process_in] because interpreting commands through [sh]
     is neither safe nor portable. *)
  let pipe_exit, pipe_entrance = Unix.pipe () in
  Unix.set_close_on_exec pipe_exit;
  let pid =
    Unix.create_process program (Array.of_list argv)
      Unix.stdin pipe_entrance Unix.stderr
  in
  Unix.close pipe_entrance;

  (* Read all that we can. *)
  let chunk_size = 1024 in
  let buf = Buffer.create chunk_size in
  let bytes = Bytes.create chunk_size in

  while
    let count = Unix.read pipe_exit bytes 0 chunk_size in
    Buffer.add_subbytes buf bytes 0 count;
    count > 0
  do
    ()
  done;

  (* Wait for child to finish. *)
  let _, status = Unix.waitpid [] pid in
  status, Buffer.contents buf

let check status =
  match status with
    | Unix.WEXITED code ->
        if code <> 0 then (
          print_endline ("Program exited with code: " ^ string_of_int code);
          exit 2
        )
    | Unix.WSIGNALED code ->
        print_endline ("Program killed by signal: " ^ string_of_int code);
        exit 3
    | Unix.WSTOPPED code ->
        print_endline ("Program stopped by signal: " ^ string_of_int code);
        exit 4

let command program arguments =
  print_endline
    (String.concat " " (List.map quote_shell (program :: arguments)));
  check (run program arguments)

let command_in program arguments =
  print_endline
    (String.concat " " (List.map quote_shell (program :: arguments)));
  let status, output = run_in program arguments in
  check status;
  output

type mode =
  | Byte
  | Native

let which_ocaml_executable name =
  let opt = name ^ ".opt" in
  match which opt with
    | Some _ ->
        Some opt
    | None ->
        match which name with
          | Some _ ->
              Some name
          | None ->
              None

let ocamlc_name = which_ocaml_executable "ocamlc"
let ocamlopt_name = which_ocaml_executable "ocamlopt"
let ocamldep_name = which_ocaml_executable "ocamldep"

let best_mode =
  match ocamlopt_name with
    | None -> Byte
    | Some _ -> Native

let byte f = f Byte
let native f = f Native
let both f = f Byte; if ocamlopt_name <> None then f Native
let best f = f best_mode

let ocaml ?(_I = []) ?(a = false) ?(c = false) ?o files mode =
  let arguments = files in
  let arguments = if a then "-a" :: arguments else arguments in
  let arguments = if c then "-c" :: arguments else arguments in
  let arguments =
    match o with
      | None -> arguments
      | Some o -> "-o" :: o :: arguments
  in
  let arguments =
    List.flatten (List.map (fun _I -> [ "-I"; _I ]) _I) @ arguments
  in
  let compiler =
    match mode with
      | Byte ->
          ocamlc_name // "ocamlc"
      | Native ->
          ocamlopt_name // "ocamlopt"
  in
  command compiler arguments

let bin_directory = Some "bin"

let compile_module ?(_I = []) ?(bin = bin_directory) name mode =
  let mli = name ^ ".mli" in
  let ml = name ^ ".ml" in
  let mli_exists = exists mli in
  let ml_exists = exists ml in

  if not mli_exists && not ml_exists then
    print_endline ("Warning: no .mli or .ml file for module: " ^ name);

  let compile_file filename =
    let bin_filename = bin //^ filename in
    let bin_parent = parent bin_filename in
    (
      match bin_parent with
        | None ->
            ()
        | Some bin_parent ->
            make_directory bin_parent
    );
    if bin <> None then link filename bin_filename;
    let _I =
      match bin_parent with
        | None ->
            _I
        | Some parent ->
            parent :: _I
    in
    ocaml ~_I ~c [ bin_filename ] mode
  in

  (* TODO: it is slightly faster to just run ocamlc once with the two files. *)
  if mli_exists then compile_file mli;
  if ml_exists then compile_file ml

type object_kind =
  | Module
  | Archive
  | External_module
  | External_archive

let object_filename ?(bin = bin_directory) mode (object_kind, name) =
  match object_kind, mode with
    | Module, Byte -> bin //^ name ^ ".cmo"
    | Module, Native -> bin //^ name ^ ".cmx"
    | Archive, Byte -> bin //^ name ^ ".cma"
    | Archive, Native -> bin //^ name ^ ".cmxa"
    | External_module, Byte -> name ^ ".cmo"
    | External_module, Native -> name ^ ".cmx"
    | External_archive, Byte -> name ^ ".cma"
    | External_archive, Native -> name ^ ".cmxa"

let link_program ?_I ?bin output objects mode =
  let objects = List.map (object_filename ?bin mode) objects in
  ocaml ?_I ~o: output objects mode

let link_library ?_I ?bin name objects mode =
  let objects = List.map (object_filename ?bin mode) objects in
  let o = object_filename ?bin mode (Archive, name) in
  ocaml ~a ?_I ~o objects mode

let simple_program ?_I ?(bin = bin_directory) ?o name =
  best @@ fun mode ->
  compile_module ?_I ~bin name mode;
  link_program ?_I (o // (bin //^ name)) [ Module, name ] mode

let simple_library ?_I ?bin name =
  both @@ fun mode ->
  compile_module ?_I ?bin name mode;
  link_library ?_I ?bin name [ Module, name ] mode

let ocamldep_modules filenames =
  let parse_line line =
    match split_string ':' line with
      | [ filename; modules ] ->
          filename, List.filter ((<>) "") (split_string ' ' modules)
      | _ ->
          failwith ("Failed to parse output of ocamldep: " ^ line)
  in
  "-modules" :: filenames
  |> command_in (ocamldep_name // "ocamldep")
  |> split_lines
  |> List.map parse_line

module String_set = Set.Make (String)
module String_map = Map.Make (String)

(* Merge [.ml] dependencies with [.mli] dependencies.
   Input: result of [ocamldep_modules], i.e. (filename, modules) pairs.
   Output: map from module filename without extension to module names. *)
let merge_module_dependencies
  (deps: (string * string list) list): String_set.t String_map.t =
  let add acc (filename, modules) =
    let base = chop_extension filename in
    let existing =
      match String_map.find base acc with
        | exception Not_found -> String_set.empty
        | set -> set
    in
    let set = List.fold_right String_set.add modules existing in
    String_map.add base set acc
  in
  List.fold_left add String_map.empty deps

let module_name filename =
  Filename.basename filename |> chop_extension |> String.capitalize

type module_dependency =
  | Internal of string (* module base *)
  | External of string (* module name *)

let show_module_dependency md =
  match md with
    | Internal base -> "Internal " ^ base
    | External name -> "External " ^ name

type module_graph = module_dependency list String_map.t

let module_graph filenames =
  (* Map internal module names to their base names. *)
  let internal_modules =
    let add acc filename =
      let base = chop_extension filename in
      String_map.add (module_name base) base acc
    in
    List.fold_left add String_map.empty filenames
  in

  let categorize_module module_name =
    match String_map.find module_name internal_modules with
      | exception Not_found ->
          External module_name
      | base ->
          Internal base
  in

  (* Map each categorized module to its dependencies. *)
  let categorize_dependencies module_names =
    String_set.elements module_names
    |> List.map categorize_module
  in
  ocamldep_modules filenames
  |> merge_module_dependencies
  |> String_map.map categorize_dependencies

module Dependency =
struct
  type t = module_dependency
  let compare = (Pervasives.compare: t -> t -> int)
end

module Dependency_set = Set.Make (Dependency)

let topological_sort (type a) ?(compare: a -> a -> int = Pervasives.compare)
    (root: a) (get_children: a -> a list): a list =
  let module Node =
  struct
    type t = a
    let compare = compare
  end
  in

  let module Node_set = Set.Make (Node) in

  let result = ref [] in
  let result_set = ref Node_set.empty in

  let rec explore seen node =
    if Node_set.mem node seen then
      failwith "Cycle detected"; (* TODO: better explanation *)

    let seen = Node_set.add node seen in
    List.iter (explore seen) (get_children node);

    if not (Node_set.mem node !result_set) then (
      result := node :: !result;
      result_set := Node_set.add node !result_set
    )
  in

  explore Node_set.empty root;
  !result

let sort_module_graph target dependencies =
  let get_children node =
    match node with
      | Internal base ->
          (
            match String_map.find base dependencies with
              | exception Not_found -> []
              | children -> children
          )
      | External _ ->
          []
  in
  topological_sort (Internal target) get_children
  |> List.rev

let compile_module_rec ?_I ?bin (graph: module_graph) base mode =
  let compile module_dependency =
    match module_dependency with
      | Internal base -> compile_module ?_I ?bin base mode
      | External _ -> ()
  in
  List.iter compile (sort_module_graph base graph)

let link_program_auto ?_I ?bin (graph: module_graph) base output mode =
  let objects =
    let get_object module_dependency =
      match module_dependency with
        | Internal base -> Some (Module, base)
        | External "Unix" -> Some (External_archive, "unix") (* TODO: param *)
        | External _ -> None
    in
    sort_module_graph base graph
    |> List.map get_object
    |> List.filter ((<>) None)
    |> List.map (function None -> assert false | Some x -> x)
  in
  link_program ?_I ?bin output objects mode

let objects_to_link (graph: module_graph) base =
  let get_object module_dependency =
    match module_dependency with
      | Internal base -> Some (Module, base)
      | External "Unix" -> Some (External_archive, "unix")
      | External _ -> None
  in
  sort_module_graph base graph
  |> list_filter_map get_object

let link_program_auto ?_I ?bin output (graph: module_graph) base mode =
  let objects = objects_to_link graph base in
  link_program ?_I ?bin output objects mode

let program ?_I ?bin output graph base mode =
  (* TODO: here we are computing the topological sort twice.
     Not a big deal, it's probably fast, but we need to check.
     Also, in parallel builds we would not compute it twice anyway? *)
  compile_module_rec graph base mode;
  link_program_auto output graph base mode

let directory dir =
  module_graph (ls dir)

let directories dirs =
  module_graph (List.flatten (List.map ls dirs))
