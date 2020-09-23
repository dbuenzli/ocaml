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
(** [get_lib_resolver ()] is a library resolver initialized with
    {!Clflags.rev_ocamlpath} and {!Config.ocamlpath} *)

val get_libs :
  ?prune:Lib.Name.Set.t -> Lib.Resolver.t -> Lib.Name.t list -> Lib.t list
(** [get_libs ~prune r names] are the library names [names] resolved
    in [r], in order. Elements of [names] that are in [prune]
    (defaults to {!Lib.Name.Set.empty}) are omitted from the result.
    Errors with {!Compenv.fatal} on resolution errors. *)

val init_path : ?dir:string -> unit -> libs:Lib.t list -> unit
val initial_env : unit -> Env.t

(* Support for flags that can also be set from an environment variable *)
val set_from_env : 'a option ref -> 'a Clflags.env_reader -> unit
val read_clflags_from_env : unit -> unit

val with_ppf_dump : file_prefix:string -> (Format.formatter -> 'a) -> 'a
