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
val dir_install_printer : formatter -> Longident.t -> unit
val dir_remove_printer : formatter -> Longident.t -> unit
val dir_trace : formatter -> Longident.t -> unit
val dir_untrace : formatter -> Longident.t -> unit
val dir_untrace_all : formatter -> unit -> unit

type 'a printer_type_new = Format.formatter -> 'a -> unit
type 'a printer_type_old = 'a -> unit

(* For topmain.ml. Maybe shouldn't be there *)
val load_file : formatter -> string -> bool

val loaded_files : unit -> Set.Make(String).t
(** [loaded_files ()] is the set of object and archive files that were
    loaded so far. *)

val loaded_libs : unit -> Set.Make(String).t
(** [loaded_libs ()] is the set of libraries that were loaded so far. *)

val assume_lib_loaded : string -> (unit, string) result
(** [assume_lib_loaded n] assumes library name [n] is loaded. This
    will prevent [n] from being looked up. An error is returned if
    [n] is not a valid library name. *)
