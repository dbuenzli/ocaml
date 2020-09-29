#3 "otherlibs/dynlink/dynlink.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*              Mark Shinwell and Leo White, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Dynlink_compilerlibs

module Bytecode = struct
  type filename = string

  module Unit_header = struct
    type t = Cmo_format.compilation_unit

    let name (t : t) = t.cu_name
    let crc _t = None

    let interface_imports (t : t) = t.cu_imports
    let implementation_imports (t : t) =
      let required =
        t.cu_required_globals
        @ Symtable.required_globals t.cu_reloc
      in
      let required =
        List.filter
          (fun id ->
             not (Ident.is_predef id)
             && not (String.contains (Ident.name id) '.'))
          required
      in
      List.map
        (fun ident -> Ident.name ident, None)
        required

    let defined_symbols (t : t) =
      List.map (fun ident -> Ident.name ident)
        (Symtable.defined_globals t.cu_reloc)

    let unsafe_module (t : t) = t.cu_primitives <> []
  end

  type handle = Stdlib.in_channel * filename * Digest.t * string list (* dlls *)

  let default_crcs = ref []
  let default_libs = ref Lib.Name.Set.empty
  let default_global_map = ref Symtable.empty_global_map

  let init () =
    if !Sys.interactive then begin (* PR#6802 *)
      invalid_arg "The dynlink.cma library cannot be used \
        inside the OCaml toplevel"
    end;
    let crcs, lib_names = Symtable.init_toplevel () in
    default_crcs := crcs;
    default_libs := lib_names;
    default_global_map := Symtable.current_state ()

  let is_native = false
  let adapt_filename f = f

  let num_globals_inited () =
    Misc.fatal_error "Should never be called for bytecode dynlink"

  let fold_initial_units ~init ~f =
    List.fold_left (fun acc (comp_unit, interface) ->
        let id = Ident.create_persistent comp_unit in
        let defined =
          Symtable.is_defined_in_global_map !default_global_map id
        in
        let implementation =
          if defined then Some (None, Dynlink_types.Loaded)
          else None
        in
        let defined_symbols =
          if defined then [comp_unit]
          else []
        in
        f acc ~comp_unit ~interface ~implementation ~defined_symbols)
      init
      !default_crcs

  let fold_initial_libs acc f =
    let add_lib lib acc = f acc (Lib.Name.to_string lib) in
    Lib.Name.Set.fold add_lib !default_libs acc

  let run_shared_startup _ = ()

  let run (ic, file_name, file_digest, dlls) ~unit_header ~priv =
    let open Misc in
    begin try
      Dll.open_dlls Dll.For_execution (List.map Dll.extract_dll_name dlls)
    with exn ->
      raise (Dynlink_types.Error (Cannot_open_dynamic_library exn))
    end;
    let old_state = Symtable.current_state () in
    let compunit : Cmo_format.compilation_unit = unit_header in
    seek_in ic compunit.cu_pos;
    let code_size = compunit.cu_codesize + 8 in
    let code = LongString.create code_size in
    LongString.input_bytes_into code ic compunit.cu_codesize;
    LongString.set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
    LongString.blit_string "\000\000\000\001\000\000\000" 0
      code (compunit.cu_codesize + 1) 7;
    begin try
      Symtable.patch_object code compunit.cu_reloc;
      Symtable.check_global_initialized compunit.cu_reloc;
      Symtable.update_global_table ()
    with Symtable.Error error ->
      let new_error : Dynlink_types.linking_error =
        match error with
        | Symtable.Undefined_global s -> Undefined_global s
        | Symtable.Unavailable_primitive s -> Unavailable_primitive s
        | Symtable.Uninitialized_global s -> Uninitialized_global s
        | Symtable.Wrong_vm _ -> assert false
      in
      raise (Dynlink_types.Error (Linking_error (file_name, new_error)))
    end;
    (* PR#5215: identify this code fragment by
       digest of file contents + unit name.
       Unit name is needed for .cma files, which produce several code
       fragments. *)
    let digest = Digest.string (file_digest ^ compunit.cu_name) in
    let events =
      if compunit.cu_debug = 0 then [| |]
      else begin
        seek_in ic compunit.cu_debug;
        [| input_value ic |]
      end in
    if priv then Symtable.hide_additions old_state;
    let _, clos = Meta.reify_bytecode code events (Some digest) in
    try ignore ((clos ()) : Obj.t)
    with exn ->
      Printexc.raise_with_backtrace
        (Dynlink_types.Error (Library's_module_initializers_failed exn))
        (Printexc.get_raw_backtrace ())

  let load ~filename:file_name ~priv:_ =
    let ic = open_in_bin file_name in
    let file_digest = Digest.channel ic (-1) in
    seek_in ic 0;
    try
      let buffer =
        try really_input_string ic (String.length Config.cmo_magic_number)
        with End_of_file ->
          raise (Dynlink_types.Error (Not_a_bytecode_file file_name))
      in
      if buffer = Config.cmo_magic_number then begin
        let handle = ic, file_name, file_digest, [] in
        let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
        seek_in ic compunit_pos;
        let cu = (input_value ic : Cmo_format.compilation_unit) in
        handle, [cu]
      end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        let lib = (input_value ic : Cmo_format.library) in
        let handle = ic, file_name, file_digest, lib.lib_dllibs in
        handle, lib.lib_units
      end else begin
        raise (Dynlink_types.Error (Not_a_bytecode_file file_name))
      end
    with exc ->
      close_in ic;
      raise exc

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    let id = Ident.create_persistent bytecode_or_asm_symbol in
    match Symtable.get_global_value id with
    | exception _ -> None
    | obj -> Some obj

  let finish (ic, _filename, _digest, _dlls) =
    close_in ic
end

include Dynlink_common.Make (Bytecode)

type linking_error = Dynlink_types.linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error = Dynlink_types.error =
  | Library_resolution_error of string (** error message *)
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error = Dynlink_types.Error
let error_message = Dynlink_types.error_message
