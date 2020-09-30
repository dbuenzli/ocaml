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

(* The toplevel directives. *)

open Format

val dir_quit : unit -> unit
val dir_directory : string -> unit
val dir_remove_directory : string -> unit
val dir_cd : string -> unit
val dir_load : formatter -> string -> unit
val dir_use : formatter -> string -> unit
val dir_use_output : formatter -> string -> unit
val dir_install_printer : formatter -> Longident.t -> unit
val dir_remove_printer : formatter -> Longident.t -> unit

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

(* For topmain.ml. Maybe shouldn't be there *)
val load_file : formatter -> string -> bool

val assume_library : string -> (unit, string) result
(** [assume_library l] declares the library [l] to be loaded.
    This prevents [l] from being looked up. An error is returned
    if [l] is not a valid library name. *)

val all_libraries : unit -> string list
(** [all_libraries ()] is the set of library names that
    are statically linked in the executable and those that
    were loaded via requires or assumed to be loaded. *)

val has_library : string -> bool
(** [has_library l] is [List.mem l (all_libraries)]. *)
