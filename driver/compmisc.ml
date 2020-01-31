(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Compenv

let get_lib_resolver =
  (* We have that as a lazy global because we'd like to create it
     as needed and share it between the explicit main and the deferred
     actions. *)
  let lib_resolver =
    lazy begin
      let ocamlpath = Lib.Ocamlpath.of_dirs Config.ocamlpath in
      Lib.Resolver.create ~ocamlpath
    end
  in
  fun () -> Lazy.force lib_resolver

let get_required_libs ?(prune = Lib.Name.Set.empty) r =
  let get r n = match Lib.Name.Set.mem n prune with
  | true -> None
  | false ->
    match Lib.Resolver.get r n with
    | Ok l -> Some l | Error e -> Compenv.fatal e
  in
  let requires = Lib.Name.uniquify (List.rev !Clflags.requires_rev) in
  List.filter_map (get r) requires

let get_libs_files get libs =
  let add_file acc l = match get l with
    | Ok f -> f :: acc | Error e -> Compenv.fatal e
  in
  List.rev (List.fold_left add_file [] libs)

(* Initialize the search path.
   [dir] is always searched first (default: the current directory),
   then the directories specified with the -I option (in command-line order),
   then library directories of [libs], usually these libraries
     are the ones specified via [-require] options (in cli order).
   then the standard library directory (unless the -nostdlib option is given).
 *)

let init_path ?(dir="") () ~libs =
  let dirs =
    let lib_dirs = List.map Lib.dir libs in
    let dirs = List.rev_append lib_dirs !Clflags.include_dirs in
    if !Clflags.use_threads then "+threads" :: dirs else dirs
  in
  let dirs =
    !last_include_dirs @ dirs @ Config.flexdll_dirs @ !first_include_dirs
  in
  let exp_dirs =
    List.map (Misc.expand_directory Config.standard_library) dirs in
  Load_path.init (dir :: List.rev_append exp_dirs (Clflags.std_include_dir ()));
  Env.reset_cache ()

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#8227) *)

let initial_env () =
  Ident.reinit();
  let initially_opened_module =
    if !Clflags.nopervasives then
      None
    else
      Some "Stdlib"
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
    ~safe_string:(Config.safe_string || not !Clflags.unsafe_string)
    ~initially_opened_module
    ~open_implicit_modules:(List.rev !Clflags.open_modules)

let set_from_env flag Clflags.{ parse; usage; env_var } =
  try
    match parse (Sys.getenv env_var) with
    | None ->
        Location.prerr_warning Location.none
          (Warnings.Bad_env_variable (env_var, usage))
    | Some x -> match !flag with
      | None -> flag := Some x
      | Some _ -> ()
  with
    Not_found -> ()

let read_clflags_from_env () =
  set_from_env Clflags.color Clflags.color_reader;
  set_from_env Clflags.error_style Clflags.error_style_reader;
  ()

let with_ppf_dump ~file_prefix f =
  let ppf_dump, finally =
    if not !Clflags.dump_into_file
    then Format.err_formatter, ignore
    else
       let ch = open_out (file_prefix ^ ".dump") in
       let ppf = Format.formatter_of_out_channel ch in
       ppf,
       (fun () ->
         Format.pp_print_flush ppf ();
         close_out ch)
  in
  Misc.try_finally (fun () -> f ppf_dump) ~always:finally
