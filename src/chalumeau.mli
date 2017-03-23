(** Build library. *)

module String_set: Set.S with type elt = string
module String_map: Map.S with type key = string

(** Value [true], to use as [~a] with functions such as {!ocamlc}. *)
val a: bool

(** Value [true], to use as [~c] with functions such as {!ocamlc}. *)
val c: bool

(** Value [true], to use as [~p] with functions such as {!mkdir}. *)
val p: bool

(** Value [true], to use as [~s] with functions such as {!ln}. *)
val s: bool

(** Value [true], to use as [~f] with functions such as {!ln}. *)
val f: bool

(** Quote a string using shell syntax. *)
val quote_shell: string -> string

(** Split a string on a character.

    Always return at least one string. *)
val split_string: char -> string -> string list

(** Read an option, with a default value. *)
val (//): 'a option -> 'a -> 'a

(** Same as [Filename.concat. *)
val (/^): string -> string -> string

(** Concatenate an optional path and another path. *)
val (//^): string option -> string -> string

(** Remove last extension if any.

    Same as [Filename.chop_extension], but return the argument if it has no
    extension instead of raising an exception. *)
val chop_extension: string -> string

(** Test if file exists.

    Same as [Sys.file_exists], but return [false] instead of raising an
    exception in the rare cases that [Sys.file_exists] does. *)
val exists: string -> bool

(** Search for a file in the [PATH]. *)
val which: string -> string option

(** Get the parent directory of a path.

    Same as [Filename.dirname], but return [None] if the path has no parent. *)
val parent: string -> string option

(** Convert a path to an absolute path. *)
val absolute: string -> string

(** Create a link.

    Create a symbolic link from [name] to [target] using: [ln ~s target name].

    Create a hard link from [name] to [target] using: [ln target name].

    Use [f] to remove [name] first if it already exists. *)
val ln: ?s: bool -> ?f: bool -> string -> string -> unit

(** Create a symbolic link if needed.

    Same as [ln ~s ~f], but also print the command and make the target path
    absolute.

    If file already exists and is a symbolic link to the same target,
    do nothing. *)
val link: string -> string -> unit

(** Create a directory. *)
val mkdir: ?p: bool -> string -> unit

(** Create a directory if it does not exist, and print the command.

    This also creates parent directories if needed, like [mkdir ~p]. *)
val make_directory: string -> unit

(** Read the contents of a directory.

    Usage: [read_directory "dir"]

    Return a result similar to [ls dir/*]: all filenames are prefixed with
    [dir]. But hidden files are returned, except [.] and [..]. *)
val ls: string -> string list

(** Run an external program and wait for it to return.

    Usage: [run program arguments]

    Example: [run "ocamlc" [ "-c"; "main.ml" ]] *)
val run: string -> string list -> Unix.process_status

(** Exit if a process did not return exit code [0]. *)
val check: Unix.process_status -> unit

(** Combination of {!run} and {!check}, which also prints the command. *)
val command: string -> string list -> unit

(** OCaml compilation modes: bytecode or native code. *)
type mode =
  | Byte
  | Native

(** One of [ocamlc.opt] or [ocamlc], depending on what is available. *)
val ocamlc_name: string option

(** One of [ocamlopt.opt] or [ocamlopt], depending on what is available. *)
val ocamlopt_name: string option

(** One of [ocamldep.opt] or [ocamldep], depending on what is available. *)
val ocamldep_name: string option

(** [Native] if [ocamlopt] is available, [Byte] otherwise. *)
val best_mode: mode

(** Call a function with [Byte]. *)
val byte: (mode -> unit) -> unit

(** Call a function with [Native]. *)
val native: (mode -> unit) -> unit

(** Call a function with [Byte], then with [Native] if [ocamlopt] is
    available. *)
val both: (mode -> unit) -> unit

(** Call a function with [Native] if [ocamlopt] is available, else
    with [Byte]. *)
val best: (mode -> unit) -> unit

(** Use {!command} to run [ocamlc] or [ocamlopt]. *)
val ocaml:
  ?_I: string list ->
  ?a: bool ->
  ?c: bool ->
  ?o: string ->
  string list -> mode -> unit

(** Compile a module.

    Usage: [compile_module mode name]

    Compile [name ^ ".mli"] and [name ^ ".ml"] if they exist.

    Return the name of the output file ([name ^ ".cmo"] or [name ^ ".cmx"]).

    If [bin] is [Some dir], output files in [dir]. Default is [Some "bin"]. *)
val compile_module:
  ?_I: string list ->
  ?bin: string option ->
  string -> mode -> unit

(** OCaml compiled object kinds. *)
type object_kind =
  | Module
  | Archive
  | External_module
  | External_archive

(** Get the filename corresponding to an compiled object.

    External objects are not prefixed with [bin].

    Usage: [object_filename mode (kind, name)]

    Example: [object_filename Native (Archive, "src/main")]
    is equal to ["bin/src/main.cmxa"]. *)
val object_filename:
  ?bin: string option ->
  mode -> object_kind * string -> string

(** Call the OCaml compiler to link objects into an executable.

    Usage: [link_program output objects mode]

    Example: [link_program "main" [ Module, "src/main" ] Byte]

    Prefix [bin] is not applied to [output]. *)
val link_program:
  ?_I: string list ->
  ?bin: string option ->
  string -> (object_kind * string) list -> mode -> unit

(** Call the OCaml compiler to link objects into a library.

    Usage: [link_library output_name objects mode]

    Example: [link_library "lib" [ Module, "src/lib" ] Byte]

    Prefix [bin] is applied to [output_name] and the relevant
    extension ([.cma] or [.cmxa]) is appended. *)
val link_library:
  ?_I: string list -> (* TODO: useless because we can't give what to link? *)
  ?bin: string option ->
  string -> (object_kind * string) list -> mode -> unit

(** Compile a program made of a single module with no dependencies.

    Usage: [simple_program name]

    Compile [name ^ ".ml"] to produce [o]. Default value for [o] is [name]. *)
val simple_program:
  ?_I: string list -> (* TODO: useless because we can't give what to link? *)
  ?bin: string option ->
  ?o: string ->
  string -> unit

(** Compile a library made of a single module with no dependencies.

    Usage: [simple_library name]

    Compile [name ^ ".mli"] and [name ^ ".ml"], if they exist, to make
    archive [name ^ ".cma"] or [name ^ ".cmxa"]. *)
val simple_library:
  ?_I: string list ->
  ?bin: string option ->
  string -> unit

(** Call [ocamldep] on given files.

    Return a list of [(filename, module_names)] pairs. *)
val ocamldep_modules: string list -> (string * string list) list

(** Get the module name of a filename.

    Example: [module_name "src/main.ml" = "Main"] *)
val module_name: string -> string

(** Dependencies in module graphs. *)
type module_dependency =
  | Internal of string (** module filename without extension *)
  | External of string (** module name, capitalized *)

(** Convert a module dependency to a string, for debugging. *)
val show_module_dependency: module_dependency -> string

(** Graph of module dependencies. *)
type module_graph = module_dependency list String_map.t

(** Compute the dependency graph of a list of files.

    Input files must have their extension, but the resulting map
    keys are module filenames without extension. *)
val module_graph: string list -> module_graph

(** Sort a graph topologically.

    Usage: [topological_sort root get_children]

    Use [compare] to detect cycles. Default is [Pervasives.compare].

    Result is from [root] to the leaves. *)
val topological_sort:
  ?compare: ('a -> 'a -> int) ->
  'a -> ('a -> 'a list) -> 'a list

(** Sort a dependency graph topologically.

    Usage: [sort_module_graph target dependencies]

    [target] is the filename of a module, without its extension.

    Result is from dependencies to [target]. *)
val sort_module_graph: string -> module_graph -> module_dependency list

(** Compile a module and its dependencies. *)
val compile_module_rec:
  ?_I: string list ->
  ?bin: string option ->
  module_graph -> string -> mode -> unit

(** Get the list of objects to list to compile a module.

    Example: [objects_to_link graph "src/main"]

    Result can be given to {!link_program} or {!link_library}. *)
val objects_to_link: module_graph -> string -> (object_kind * string) list

(** Same as {!link_program}, but compute objects to link from module graph.

    Usage: [link_program_auto output graph target mode] *)
val link_program_auto:
  ?_I: string list ->
  ?bin: string option ->
  string -> module_graph -> string -> mode -> unit

(** Compile and link a program.

    Dependencies are compiled recursively and linked in an order compatible
    with the dependency graph. *)
val program:
  ?_I: string list ->
  ?bin: string option ->
  string -> module_graph -> string -> mode -> unit

(** List modules from a directory and build their dependency graph. *)
val directory: string -> module_graph

(** List modules from several directories and build their dependency graph. *)
val directories: string list -> module_graph
