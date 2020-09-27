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

(* Link a set of .cmx/.o files and produce an executable *)

open Misc
open Config
open Cmx_format
open Compilenv

module String = Misc.Stdlib.String

type error =
  | Lib_resolution_error of filepath option * string
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (modname * string list) list
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Multiple_definition of modname * filepath * filepath
  | Missing_cmx of filepath * modname

exception Error of error

type entity =
  [ `Lib of Lib.Name.t | `File_and_deps of filepath | `File of filepath ]

(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = ref ([] : string list)

module Cmx_consistbl = Consistbl.Make (Misc.Stdlib.String)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)
let cmx_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc ->
            if name = unit.ui_name
            then Cmi_consistbl.set crc_interfaces name crc file_name
            else Cmi_consistbl.check crc_interfaces name crc file_name)
      unit.ui_imports_cmi
  with Cmi_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crco) ->
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations name crc file_name)
      unit.ui_imports_cmx
  with Cmx_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  begin try
    let source = List.assoc unit.ui_name !implementations_defined in
    raise (Error(Multiple_definition(unit.ui_name, file_name, source)))
  with Not_found -> ()
  end;
  implementations := unit.ui_name :: !implementations;
  Cmx_consistbl.set crc_implementations unit.ui_name crc file_name;
  implementations_defined :=
    (unit.ui_name, file_name) :: !implementations_defined;
  if unit.ui_symbol <> unit.ui_name then
    cmx_required := unit.ui_name :: !cmx_required

let extract_crc_interfaces () =
  Cmi_consistbl.extract !interfaces crc_interfaces
let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_requires = ref [] (* This is only used by [Asmlink.link_shared] which
                             behaves more like [Bytelibrarian.create_archive]
                             than [{Asm,Byte}link.link]. *)
let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  lib_requires := l.lib_requires :: !lib_requires;
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end

let runtime_lib () =
  let libname = "libasmrun" ^ !Clflags.runtime_variant ^ ext_lib in
  try
    if !Clflags.nopervasives || not !Clflags.with_runtime then []
    else [ Load_path.find libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

(* First pass: determine which units are needed *)

let missing_globals = (Hashtbl.create 17 : (string, string list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, _crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  Hashtbl.iter (fun md rq -> mg := (md, !rq) :: !mg) missing_globals;
  !mg

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let object_file_name_of_file = function
  | Unit (fname, _, _) -> Some (Filename.chop_suffix fname ".cmx" ^ ext_obj)
  | Library (fname, infos) ->
      let obj_file = Filename.chop_suffix fname ".cmxa" ^ ext_lib in
      (* MSVC doesn't support empty .lib files, and macOS struggles to make
         them (#6550), so there shouldn't be one if the .cmxa contains no
         units. The file_exists check is added to be ultra-defensive for the
         case where a user has manually added things to the .a/.lib file *)
      if infos.lib_units = [] && not (Sys.file_exists obj_file) then None else
      Some obj_file

let read_file obj_name =
  let file_name =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let find_lib_cmxa r (`Lib n | `Lib_from_cmxa (n, _) as l) =
  let err e =
    let src = match l with `Lib _ -> None | `Lib_from_cmxa (_, f) -> Some f in
    raise (Error (Lib_resolution_error (src, e)))
  in
  match Lib.Resolver.get r n with
  | Error e -> err e
  | Ok lib -> match Lib.cmxa lib with Error e -> err e | Ok cmxa -> cmxa

let rec read_cmxa_deps file infos r lib_seen rev_infos =
  let libs = List.map (fun n -> `Lib_from_cmxa (n, file)) infos.lib_requires in
  read_obj_infos_rev r lib_seen rev_infos libs

and read_obj_infos_rev r lib_seen rev_infos = function
  | [] -> lib_seen, rev_infos
  | ((`Lib n | `Lib_from_cmxa (n, _)) as lib) :: entities ->
      begin match Lib.Name.Set.mem n lib_seen with
      | true -> read_obj_infos_rev r lib_seen rev_infos entities
      | false ->
          let lib_seen = Lib.Name.Set.add n lib_seen in
          let cmxa = `File_and_deps (find_lib_cmxa r lib) in
          read_obj_infos_rev r lib_seen rev_infos (cmxa :: entities)
      end
  | ((`File file_name | `File_and_deps file_name) as file) :: entities ->
      match read_file file_name with
      | Unit _ as cmx ->
          read_obj_infos_rev r lib_seen (cmx :: rev_infos) entities
      | Library (fname, infos) as cmxa ->
          let lib_seen, rev_infos = match file with
            | `File _ -> lib_seen, rev_infos
            | `File_and_deps _ ->
                (* Before adding the info for the cma to [rev_infos] we add the
                   info for the cmas of the libraries it requires; for those not
                   already seen and recursively. Implicitely this makes a stable
                   topological sort of the cmas and their required libraries by
                   depth first exploration of the library dependency DAG.
                   Not TR but bound by depth of the dependency DAG. *)
                read_cmxa_deps fname infos r lib_seen rev_infos
          in
          read_obj_infos_rev r lib_seen (cmxa :: rev_infos) entities

let read_obj_infos r lib_seen entities =
  let lib_seen, rev_infos = read_obj_infos_rev r lib_seen [] entities in
  lib_seen, List.rev rev_infos

let scan_file file tolink = match file with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      remove_required info.ui_name;
      List.iter (add_required file_name) info.ui_imports_cmx;
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      List.fold_right
        (fun (info, crc) reqd ->
           if info.ui_force_link
           || !Clflags.link_everything
           || is_required info.ui_name
           then begin
             remove_required info.ui_name;
             List.iter (add_required (Printf.sprintf "%s(%s)"
                                        file_name info.ui_name))
               info.ui_imports_cmx;
             (info, file_name, crc) :: reqd
           end else
           reqd)
        infos.lib_units tolink

(* Second pass: generate the startup file and link it with everything else *)

let force_linking_of_startup ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata ([Cmm.Csymbol_address "caml_startup"]))

let make_globals_map units_list ~crc_interfaces =
  let crc_interfaces = String.Tbl.of_seq (List.to_seq crc_interfaces) in
  let defined =
    List.map (fun (unit, _, impl_crc) ->
        let intf_crc = String.Tbl.find crc_interfaces unit.ui_name in
        String.Tbl.remove crc_interfaces unit.ui_name;
        (unit.ui_name, intf_crc, Some impl_crc, unit.ui_defines))
      units_list
  in
  String.Tbl.fold (fun name intf acc ->
      (name, intf, None, []) :: acc)
    crc_interfaces defined

let make_startup_file ~ppf_dump units_list ~crc_interfaces =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  Compilenv.reset "_startup";
  (* set the name of the "current" compunit *)
  Emit.begin_assembly ();
  let name_list =
    List.flatten (List.map (fun (info,_,_) -> info.ui_defines) units_list) in
  compile_phrase (Cmm_helpers.entry_point name_list);
  let units = List.map (fun (info,_,_) -> info) units_list in
  List.iter compile_phrase (Cmm_helpers.generic_functions false units);
  Array.iteri
    (fun i name -> compile_phrase (Cmm_helpers.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmm_helpers.global_table name_list);
  let globals_map = make_globals_map units_list ~crc_interfaces in
  compile_phrase (Cmm_helpers.globals_map globals_map);
  compile_phrase(Cmm_helpers.data_segment_table ("_startup" :: name_list));
  if !Clflags.function_sections then
    compile_phrase
      (Cmm_helpers.code_segment_table("_hot" :: "_startup" :: name_list))
  else
    compile_phrase(Cmm_helpers.code_segment_table("_startup" :: name_list));
  let all_names = "_startup" :: "_system" :: name_list in
  compile_phrase (Cmm_helpers.frame_table all_names);
  if Config.spacetime then begin
    compile_phrase (Cmm_helpers.spacetime_shapes all_names);
  end;
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  Emit.end_assembly ()

let make_shared_startup_file ~ppf_dump units ~requires =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup";
  Compilenv.reset "_shared_startup";
  Emit.begin_assembly ();
  List.iter compile_phrase
    (Cmm_helpers.generic_functions true (List.map fst units));
  compile_phrase (Cmm_helpers.plugin_header units ~requires);
  compile_phrase
    (Cmm_helpers.global_table
       (List.map (fun (ui,_) -> ui.ui_symbol) units));
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)
  Emit.end_assembly ()

let call_linker_shared file_list output_name =
  let exitcode = Ccomp.call_linker Ccomp.Dll output_name file_list "" in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let link_shared ~ppf_dump ~requires objfiles output_name =
  Profile.record_call output_name (fun () ->
    let obj_infos = List.map read_file objfiles in
    let units_tolink = List.fold_right scan_file obj_infos [] in
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles =
      List.rev (List.filter_map object_file_name_of_file obj_infos) @
      (List.rev !Clflags.ccobjs) in
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    let requires = List.(concat (rev (requires :: !lib_requires))) in
    Asmgen.compile_unit
      startup !Clflags.keep_startup_file startup_obj
      (fun () ->
         make_shared_startup_file ~ppf_dump
           (List.map (fun (ui,_,crc) -> (ui,crc)) units_tolink)
           ~requires
      );
    call_linker_shared (startup_obj :: objfiles) output_name;
    remove_file startup_obj
  )

let call_linker file_list startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
  in
  let files = startup_file :: (List.rev file_list) in
  let libunwind =
    if not Config.spacetime then []
    else if not Config.libunwind_available then []
    else String.split_on_char ' ' Config.libunwind_link_flags
  in
  let files, c_lib =
    if (not !Clflags.output_c_object) || main_dll || main_obj_runtime then
      files @ (List.rev !Clflags.ccobjs) @ runtime_lib () @ libunwind,
      (if !Clflags.nopervasives || (main_obj_runtime && not main_dll)
       then "" else Config.native_c_libraries)
    else
      files, ""
  in
  let mode =
    if main_dll then Ccomp.MainDll
    else if !Clflags.output_c_object then Ccomp.Partial
    else Ccomp.Exe
  in
  let exitcode = Ccomp.call_linker mode output_name files c_lib in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

(* Main entry point *)

let link ~ppf_dump r ~assume_libs (entities : entity list) output_name =
  Profile.record_call output_name (fun () ->
    let stdlib = `File "stdlib.cmxa" in
    let stdexit = `File "std_exit.cmx" in
    let entities =
      ((if !Clflags.nopervasives then entities
       else if !Clflags.output_c_object then stdlib :: entities
       else stdlib :: entities @ [stdexit])
       :> [ entity | `Lib_from_cmxa of Lib.Name.t * filepath ] list)
    in
    let _lib_seen, obj_infos = read_obj_infos r assume_libs entities in
    let units_tolink = List.fold_right scan_file obj_infos [] in
    Array.iter remove_required Runtimedef.builtin_exceptions;
    begin match extract_missing_globals() with
      [] -> ()
    | mg -> raise(Error(Missing_implementations mg))
    end;
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    let crc_interfaces = extract_crc_interfaces () in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                 (* put user's opts first *)
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = Filename.temp_file "camlstartup" ext_obj in
    Asmgen.compile_unit
      startup !Clflags.keep_startup_file startup_obj
      (fun () -> make_startup_file ~ppf_dump units_tolink ~crc_interfaces);
    Misc.try_finally
      (fun () ->
         call_linker (List.filter_map object_file_name_of_file obj_infos)
           startup_obj output_name)
      ~always:(fun () -> remove_file startup_obj)
  )

(* Error report *)

open Format

let report_error ppf = function
  | Lib_resolution_error (erroring_cmxa, err) ->
      begin match erroring_cmxa with
      | None -> fprintf ppf "@[%s@]" err
      | Some file ->
          fprintf ppf "@[%s@]" (Lib.Resolver.err_in_archive ~file err)
      end
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.print_filename name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           fprintf ppf "%s" r1;
           List.iter (fun r -> fprintf ppf ",@ %s" r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
            fprintf ppf "@ @[<hov 2>%s referenced from %a@]" md
            print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a" Location.print_filename file
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %s@]"
        Location.print_filename file1
        Location.print_filename file2
        modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmx file@ for module %s,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %s.cmx@ is found.@]"
        Location.print_filename filename name
        Location.print_filename  filename
        name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  implementations_defined := [];
  cmx_required := [];
  interfaces := [];
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []
