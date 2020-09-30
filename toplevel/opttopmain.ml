(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags

let usage =
   "Usage: ocamlnat <options> <object-files> [script-file]\noptions are:"

let preload_objects = ref []

(* Position of the first non expanded argument *)
let first_nonexpanded_pos = ref 0

let current = ref (!Arg.current)

let argv = ref Sys.argv

(* Test whether the option is part of a responsefile *)
let is_expanded pos = pos < !first_nonexpanded_pos

let expand_position pos len =
  if pos < !first_nonexpanded_pos then
    (* Shift the position *)
    first_nonexpanded_pos := !first_nonexpanded_pos + len
  else
    (* New last position *)
    first_nonexpanded_pos := pos + len + 2

let prepare ppf =
  Opttoploop.set_paths ();
  try
    let assume n = ignore (Opttopdirs.assume_library (Lib.Name.to_string n)) in
    let require_to_string = function
      | `Lib l -> Lib.Name.to_string l | `File_and_deps a -> a
    in
    let require r = Opttopdirs.require ppf (require_to_string r) in
    let load_obj obj = Opttopdirs.load_file ppf obj in
    let assume_libs = Lib.Name.Set.elements @@ Compenv.get_assume_libs () in
    let reqs = Compenv.get_requires () in
    let objs = List.rev (!preload_objects @ !Compenv.first_objfiles) in
    let () = List.iter assume assume_libs in
    (* XXX this is not according to RFC we need to respect objs/requires
       interleaving. Doing that cleanly remains a question at the moment. *)
    let res = List.for_all require reqs && List.for_all load_obj objs in
    Opttoploop.run_hooks Opttoploop.Startup;
    res
  with x ->
    try Location.report_exception ppf x; false
    with x ->
      Format.fprintf ppf "Uncaught exception: %s\n" (Printexc.to_string x);
      false

let file_argument name =
  let ppf = Format.err_formatter in
  if Filename.check_suffix name ".cmxs"
    || Filename.check_suffix name ".cmx"
    || Filename.check_suffix name ".cmxa"
  then preload_objects := name :: !preload_objects
  else if is_expanded !current then begin
    (* Script files are not allowed in expand options because otherwise the
       check in override arguments may fail since the new argv can be larger
       than the original argv.
    *)
    Printf.eprintf "For implementation reasons, the toplevel does not support\
    \ having script files (here %S) inside expanded arguments passed through\
    \ the -args{,0} command-line option.\n" name;
    exit 2
  end else begin
    let newargs = Array.sub !argv !Arg.current
                              (Array.length !argv - !Arg.current)
      in
      Compmisc.read_clflags_from_env ();
      if prepare ppf && Opttoploop.run_script ppf name newargs
      then exit 0
      else exit 2
    end

let wrap_expand f s =
  let start = !current in
  let arr = f s in
  expand_position start (Array.length arr);
  arr

module Options = Main_args.Make_opttop_options (struct
    include Main_args.Default.Opttopmain
    let _stdin () = file_argument ""
    let _args = wrap_expand Arg.read_arg
    let _args0 = wrap_expand Arg.read_arg0
    let anonymous s = file_argument s
end);;

let () =
  let extra_paths =
    match Sys.getenv "OCAMLTOP_INCLUDE_PATH" with
    | exception Not_found -> []
    | s -> Misc.split_path_contents s
  in
  Clflags.include_dirs := List.rev_append extra_paths !Clflags.include_dirs

let main () =
  native_code := true;
  let list = ref Options.list in
  begin
    try
      Arg.parse_and_expand_argv_dynamic current argv list file_argument usage;
    with
    | Arg.Bad msg -> Format.fprintf Format.err_formatter "%s%!" msg; exit 2
    | Arg.Help msg -> Format.fprintf Format.std_formatter "%s%!" msg; exit 0
  end;
  Compmisc.read_clflags_from_env ();
  if not (prepare Format.err_formatter) then exit 2;
  let libs =
    let assumed = Compenv.get_assume_libs () in
    let prune = Lib.Name.Set.union Opttoploop.main_program_libraries assumed in
    let lib = function `Lib l -> Some l | `File_and_deps _ -> None in
    let libs = List.filter_map lib (Compenv.get_requires ()) in
    Compmisc.get_libs ~prune (Compmisc.get_lib_resolver ()) libs
  in
  Compmisc.init_path () ~libs;
  Opttoploop.loop Format.std_formatter
