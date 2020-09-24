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

open Misc

(* Link .cmo files and produce a bytecode executable. *)

type entity =
  [ `Lib of Lib.Name.t (** Library (by name) *)
  | `File_and_deps of filepath (** File and its library dependencies. *)
  | `File of filepath (** File without library dependencies. *) ]
(** The type for entities to link. *)

val link :
  Lib.Resolver.t -> assume_libs:Lib.Name.Set.t -> entity list -> filepath ->
  unit
(** [link r ~assume_libs es out_file] links entities [es] and outputs
    the result to [out_file]. [r] is used to resolve library names to
    cma files, except for those names that are in [assume_libs] which
    are not resolved. [es] is given in link order, however resolved
    libraries are (re)sorted in stable topological order according to
    their dependencies. *)

val reset : unit -> unit

val check_consistency: filepath -> Cmo_format.compilation_unit -> unit

val extract_crc_interfaces: unit -> crcs

type error =
  | Lib_resolution_error of
      filepath option * string (** erroring file (if any) and error *)
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Wrong_object_name of filepath
  | Symbol_error of filepath * Symtable.error
  | Inconsistent_import of modname * filepath * filepath
  | Custom_runtime
  | File_exists of filepath
  | Cannot_open_dll of filepath
  | Required_module_unavailable of modname * modname
  | Camlheader of string * filepath

exception Error of error

open Format

val report_error: formatter -> error -> unit
