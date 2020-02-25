#3 "otherlibs/dynlink/native/dynlink.ml"
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

(* Dynamic loading of .cmx files *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Dynlink_compilerlibs

type global_map = {
  name : string;
  crc_intf : Digest.t option;
  crc_impl : Digest.t option;
  syms : string list
}

module Native = struct
  type handle

  external ndl_open : string -> bool -> handle * Cmxs_format.dynheader
    = "caml_natdynlink_open"
  external ndl_run : handle -> string -> unit = "caml_natdynlink_run"
  external ndl_getmap : unit -> global_map list = "caml_natdynlink_getmap"
  external ndl_imported_libs : unit -> Lib.Name.Set.t =
    "caml_natdynlink_imported_libs"
  external ndl_globals_inited : unit -> int = "caml_natdynlink_globals_inited"
  external ndl_loadsym : string -> Obj.t = "caml_natdynlink_loadsym"

  module Unit_header = struct
    type t = Cmxs_format.dynunit

    let name (t : t) = t.dynu_name
    let crc (t : t) = Some t.dynu_crc

    let interface_imports (t : t) = t.dynu_imports_cmi
    let implementation_imports (t : t) = t.dynu_imports_cmx

    let defined_symbols (t : t) = t.dynu_defines
    let unsafe_module _t = false
  end

  let init () = ()

  let is_native = true
  let adapt_filename f = Filename.chop_extension f ^ ".cmxs"

  let num_globals_inited () = ndl_globals_inited ()

  let fold_initial_units ~init ~f =
    let rank = ref 0 in
    List.fold_left (fun acc { name; crc_intf; crc_impl; syms; } ->
        rank := !rank + List.length syms;
        let implementation =
          match crc_impl with
          | None -> None
          | Some _ as crco -> Some (crco, Dynlink_types.Check_inited !rank)
        in
        f acc ~comp_unit:name ~interface:crc_intf
            ~implementation ~defined_symbols:syms)
      init
      (ndl_getmap ())

  let fold_initial_libs acc f =
    let add_lib lib acc = f acc (Lib.Name.to_string lib) in
    Lib.Name.Set.fold add_lib (ndl_imported_libs ()) acc

  let run_shared_startup handle =
    ndl_run handle "_shared_startup"

  let run handle ~unit_header ~priv:_ =
    List.iter (fun cu ->
        try ndl_run handle cu
        with exn ->
          Printexc.raise_with_backtrace
            (Dynlink_types.Error (Library's_module_initializers_failed exn))
            (Printexc.get_raw_backtrace ()))
      (Unit_header.defined_symbols unit_header)

  let load ~filename ~priv =
    let handle, header =
      try ndl_open filename (not priv)
      with exn -> raise (Dynlink_types.Error (Cannot_open_dynamic_library exn))
    in
    if header.dynu_magic <> Config.cmxs_magic_number then begin
      raise (Dynlink_types.Error (Not_a_bytecode_file filename))
    end;
    handle, header.dynu_units

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    match ndl_loadsym bytecode_or_asm_symbol with
    | exception _ -> None
    | obj -> Some obj

  let finish _handle = ()
end

include Dynlink_common.Make (Native)

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
