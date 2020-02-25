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

module String = Misc.Stdlib.String

type error =
  | Lib_resolution_error of filepath * string
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (modname * string list) list
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error
  | Multiple_definition of modname * filepath * filepath
  | Missing_cmx of filepath * modname

exception Error of error

(* Read native code objects information *)

type obj_info =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let rec read_objs_infos ~lib_resolver lib_seen cmxa_seen rev_infos = function
  | [] -> lib_seen, cmxa_seen, rev_infos
  | obj_name :: obj_names ->
      let file_name =
        try Load_path.find obj_name
        with Not_found -> raise (Error (File_not_found obj_name))
      in
      if Filename.check_suffix file_name ".cmx" then
        begin
          let (info, crc) = Compilenv.read_unit_info file_name in
          let rev_infos = Unit (file_name, info, crc) :: rev_infos in
          read_objs_infos ~lib_resolver lib_seen cmxa_seen rev_infos obj_names
        end
      else if Filename.check_suffix file_name ".cmxa" then
        match lib_resolver with
        (* This case is for [Asmlink.link_share], the lib_requires of cmxa
           files are not resolved. Instead they are added to the final
           cmxs metadata via add_ccobjs during [link_obj], unless
           [-noautoliblink] is specified. Note that this execution
           path does not care about or update [lib_seen] and [cmxa_seen]. *)
        | None ->
            let infos =
              try Compilenv.read_library_info file_name
              with Compilenv.Error (Not_a_unit_info _) ->
                raise (Error (Not_an_object_file file_name))
            in
            let rev_infos = Library (file_name, infos) :: rev_infos in
            read_objs_infos ~lib_resolver lib_seen cmxa_seen rev_infos obj_names
        (* These cases are for [Asmlink.link]. *)
        | Some _ when Stdlib.String.Set.mem file_name cmxa_seen ->
            read_objs_infos ~lib_resolver lib_seen cmxa_seen rev_infos obj_names
        | Some r ->
            let infos =
              try Compilenv.read_library_info file_name
              with Compilenv.Error (Not_a_unit_info _) ->
                raise (Error (Not_an_object_file file_name))
            in
            let cmxa_seen = Stdlib.String.Set.add file_name cmxa_seen in
            let lib_seen, cmxa_seen, rev_infos =
              (* Before adding the info for the cmxa to [rev_infos] we
                 add the info for the cmxas of the libraries it requires;
                 for those not already seen and recursively and if
                 [-noautoliblink] is not specified. Implicitely
                 this makes a stable topological sort of the cmxas and their
                 required libraries by depth first exploration of the library
                 dependency DAG. The recursive call to [read_obj_infos] below
                 makes the procedure not tailrec but stack size is bound by
                 depth of the library dependency DAG. *)
              if !Clflags.no_auto_lib_link
              then lib_seen, cmxa_seen, rev_infos else
              let add_lib_require_cmxa acc n =
                let err cmxa e =
                  raise (Error (Lib_resolution_error (cmxa, e)))
                in
                match Lib.Resolver.get r n with
                | Error e -> err file_name e
                | Ok l ->
                    match Lib.cmxa l with
                    | Error e -> err file_name e
                    | Ok cmxa when Stdlib.String.Set.mem cmxa cmxa_seen -> acc
                    | Ok cmxa -> cmxa :: acc
              in
              let reqs = infos.lib_requires in
              let lib_seen = List.fold_right Lib.Name.Set.add reqs lib_seen in
              let rev_cmxas = List.fold_left add_lib_require_cmxa [] reqs in
              let cmxas = List.rev rev_cmxas in
              read_objs_infos ~lib_resolver lib_seen cmxa_seen rev_infos cmxas
            in
            let rev_infos = Library (file_name, infos) :: rev_infos in
            read_objs_infos ~lib_resolver lib_seen cmxa_seen rev_infos obj_names
      else
        raise (Error (Not_an_object_file file_name))

let obj_info_file_name = function
  | Unit (file_name, _, _) -> Filename.chop_suffix file_name ".cmx" ^ ext_obj
  | Library (file_name, _) -> Filename.chop_suffix file_name ".cmxa" ^ ext_lib

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
  if not !Clflags.no_auto_lib_link then begin
    lib_requires := l.lib_requires :: !lib_requires;
  end;
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

let link_unit file_name info crc =
  (* This is a .cmx file. It must be linked in any case. *)
  remove_required info.ui_name;
  List.iter (add_required file_name) info.ui_imports_cmx;
  (info, file_name, crc)

let link_library file_name infos tolink =
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

let link_obj tolink = function
  | Unit (file_name, info, crc) -> link_unit file_name info crc :: tolink
  | Library (file_name, infos) -> link_library file_name infos tolink

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

let make_startup_file ~ppf_dump units_list ~crc_interfaces ~lib_names  =
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
  compile_phrase (Cmm_helpers.imported_libs lib_names);
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
  if not (Ccomp.call_linker Ccomp.Dll output_name file_list "")
  then raise(Error Linking_error)

let link_shared ~ppf_dump ~requires objfiles output_name =
  Profile.record_call output_name (fun () ->
   (* [read_objs_infos] processes objfiles from left-to-right and returns
      info in reverse order. This means that the left fold with link_obj
      for link effects processes objfiles from right-to-left. *)
    let _, _, rev_obj_infos =
      let lib_resolver = None and lib_seen = Lib.Name.Set.empty
      and cmxa_seen = Stdlib.String.Set.empty in
      read_objs_infos ~lib_resolver lib_seen cmxa_seen [] objfiles
    in
    let units_tolink = List.fold_left link_obj [] rev_obj_infos in
    List.iter
      (fun (info, file_name, crc) -> check_consistency file_name info crc)
      units_tolink;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles = List.rev (List.rev_map obj_info_file_name rev_obj_infos) @
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
  if not (Ccomp.call_linker mode output_name files c_lib)
  then raise(Error Linking_error)

(* Main entry point *)

let link ~ppf_dump lib_resolver objfiles output_name =
  Profile.record_call output_name (fun () ->
    let stdlib = "stdlib.cmxa" in
    let stdexit = "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let lib_seen = Lib.Name.Set.of_list (!Clflags.requires_rev) in
   (* [read_objs_infos] processes objfiles from left-to-right and returns
      info in reverse order. This means that the left fold with link_obj
      for link effects processes objfiles from right-to-left. *)
    let lib_seen, _, rev_obj_infos =
      let lib_resolver = Some lib_resolver in
      read_objs_infos ~lib_resolver lib_seen Stdlib.String.Set.empty [] objfiles
    in
    let lib_names = if !Clflags.link_everything then Some lib_seen else None in
    let units_tolink = List.fold_left link_obj [] rev_obj_infos in
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
      (fun () ->
         make_startup_file ~ppf_dump units_tolink ~crc_interfaces ~lib_names);
    Misc.try_finally
      (fun () ->
         call_linker (List.rev_map obj_info_file_name rev_obj_infos)
           startup_obj output_name)
      ~always:(fun () -> remove_file startup_obj)
  )

(* Error report *)

open Format

let report_error ppf = function
  | Lib_resolution_error (cmxa, err) ->
      fprintf ppf "@[%s@]" (Lib.Resolver.err_in_archive ~file:cmxa err)
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
  | Linking_error ->
      fprintf ppf "Error during linking"
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
