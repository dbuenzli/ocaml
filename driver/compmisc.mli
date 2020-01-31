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

val get_lib_resolver : unit -> Lib.Resolver.t
(** [get_lib_resolver ()] is a library resolver using
    {!Config.ocamlpath}. *)

val get_required_libs : ?prune:Lib.Name.Set.t -> Lib.Resolver.t -> Lib.t list
(** [get_required_libs ~prune r] is the list of libraries that where
    [-require]d on the cli and whose name is not in [prune] (defaults
    to {!Lib.Name.Set.empty}) resolved using [r]. Errors with
    {Compenv.fatal} on resolution errors. *)

val get_libs_files :
  (Lib.t -> (Misc.filepath, string) result) -> Lib.t list ->
  Misc.filepath list
(** [get_libs_file get_file libs] maps [get_file] on [libs] and
    errors with {!Compenv.fatal} in case of error. *)

val init_path : ?dir:string -> unit -> libs:Lib.t list -> unit
val initial_env : unit -> Env.t

(* Support for flags that can also be set from an environment variable *)
val set_from_env : 'a option ref -> 'a Clflags.env_reader -> unit
val read_clflags_from_env : unit -> unit

val with_ppf_dump : file_prefix:string -> (Format.formatter -> 'a) -> 'a
