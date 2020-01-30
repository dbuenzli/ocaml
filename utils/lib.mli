(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Library names and lookups.

    As mandated by the OCaml library convention. *)

(** {1:ocamlpath The [OCAMLPATH]} *)

module Ocamlpath : sig

  val env : string
  (** [env] is the ["OCAMLPATH"] environment variable. *)

  type t
  (** The type for [OCAMLPATH]s. An ordered list of directories in which
      libraries are looked up from left to right. *)

  val of_string : string -> t
  (** [of_string s] parses [s] into an [OCAMLPATH] by following the
      build OS convention for [PATH] like variables. This means they
      are colon ':' (semi-colon ';' on Windows) separated paths. Empty
      paths are allowed and discarded. *)

  val to_string : t -> string
  (** [to_string p] formats the OCAMLPATH [p] using the build OS convention. *)

  val of_dirs : Misc.filepath list -> t
  (** [of_dirs dirs] takes the given list of directories to define
      an [OCAMLPATH]. Empty strings are dropped. *)

  val to_dirs : t -> Misc.filepath list
  (** [to_dirs p] is the list of directories in [OCAMLPATH] p, in
      lookup order. *)
end

(** {1:libs Libraries} *)

(** Library names. *)
module Name : sig

  type t
  (** The type for library names. Dot (['.']) separated non-empty
      segments. Each segment is an uncapitalized OCaml compilation
      unit name with ['-'] added to non-starter characters. *)

  val of_string : string -> (t, string) result
  (** [of_string s] is a library name from [s]. *)

  val to_string : t -> string
  (** [to_string n] is [n] as a string. *)

  val of_path : Misc.filepath -> (t, string) result
  (** [of_path s] transforms the {e relative} file path [s]
      to a library name by substituting {!Filename.dir_sep} and '/'
      with dots and verifying the segments are {{!t}valid}. *)

  val to_path : t -> Misc.filepath
  (** [to_path n] are the segments of [n] separated by {!Filename.dir_sep}. *)

  val uniquify : t list -> t list
  (** [uniquify ns] is [ns] without duplicates, the list order among
      different elements is preserved. *)

  val equal : t -> t -> bool
  (** [equal n0 n1] is [true] iff [n0] and [n1] are the same library name. *)

  val compare : t -> t -> int
  (** [compare n0 n1] is a total order on library names compatible with
      {!equal}. *)

  (** Library name sets. *)
  module Set : sig
    include Set.S with type elt = t

    val to_string_set : t -> Set.Make(String).t
    (** [to_string_set ns] is [ns] as a set of strings. *)
  end

  (** Library name maps. *)
  module Map : Map.S with type key = t
end

type t
(** The type for libraries. *)

val name : t -> Name.t
(** [name l] is the name of library [l]. *)

val dir : t -> Misc.filepath
(** [dir l] is the path to the library directory of library
    [l]. Incidentally this is also the path to the includes directory
    of the library. *)

val cma : t -> (Misc.filepath, string) result
(** [cma l] is the file path [dir l/lib.cma] if it exists. *)

val cmxa : t -> (Misc.filepath, string) result
(** [cmxa l] is the file path [dir l/lib.cmxa] if it exists. *)

val cmxs : t -> (Misc.filepath, string) result
(** [cmxs l] is the file path [dir l/lib.cmxs] if it exists. *)

(** {1:resolver Resolvers} *)

(** Library resolvers. *)
module Resolver : sig

  type lib = t
  (** The type for libraries, see {!Lib.t}. *)

  type t
  (** The type for library resolvers. *)

  val create : ocamlpath:Ocamlpath.t -> t
  (** [create ~ocamlpath] is a library resolver looking for libraries
      in the [OCAMLPATH] [ocamlpath]. A resolver assumes the file
      system structure in the directories of [ocamlpath] is immutable,
      create a new resolver if it changes. *)

  val ocamlpath : t -> Ocamlpath.t
  (** [ocamlpath r] is the [OCAMLPATH] of [r]. *)

  val find : t -> Name.t -> lib option
  (** [find r n] finds the library named [n] in [r] (if any). *)

  val get : t -> Name.t -> (lib, string) result
  (** [get r n] finds the library named [n] in [r] and errors if
      not found. *)

  val err_in_archive : file:Misc.filepath -> string -> string
  (** [err_in_archive file err] indicates [err] occured in the library
      archive [err]. *)
end
