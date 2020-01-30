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

let strf = Printf.sprintf

(* The OCAMLPATH *)

module Ocamlpath = struct
  let env_sep = if Sys.win32 then ';' else ':'
  let env = "OCAMLPATH"

  type t = Misc.filepath list (* no empty string in that list *)
  let append p0 p1 = List.append p0 p1
  let non_empty s = String.length s <> 0
  let of_string s = List.find_all non_empty (String.split_on_char env_sep s)
  let to_string dirs = String.concat (String.make 1 env_sep) dirs
  let of_dirs dirs = List.find_all non_empty dirs
  let to_dirs = Fun.id
end

(* Libraries *)

module Name = struct
  type t = string

  let err_abs_path p = strf "Absolute path %S cannot represent a library name" p
  let err_empty = "Library name cannot be empty"
  let err s i fmt = strf ("Library name %S, character %d: " ^^ fmt) s i
  let err_illegal s i ~char = err s i "illegal character %C" char
  let err_exp_letter s i ~found =
    err s i "expected 'a'...'z' letter, found %C" found

  let of_string s =
    if s = "" then Error err_empty else
    let max = String.length s - 1 in
    let rec loop i ~id_start = match i > max with
      | true -> Ok s
      | false when id_start ->
          begin match s.[i] with
          | 'a' .. 'z' -> loop (i + 1) ~id_start:false
          | c -> Error (err_exp_letter s i ~found:c)
          end
      | false ->
          begin match s.[i] with
          | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' ->
              loop (i + 1) ~id_start:false
          | '.' -> loop (i + 1) ~id_start:true
          | c -> Error (err_illegal s i ~char:c)
          end
    in
    loop 0 ~id_start:true

  let to_string = Fun.id

  let of_path p = match Filename.is_relative p with
    | false -> Error (err_abs_path p)
    | true ->
        let dir_sep = Filename.dir_sep.[0] and slash = '/' in
        let b = Bytes.of_string p in
        for i = 0 to String.length p - 1 do
          if p.[i] = dir_sep || p.[i] = slash then Bytes.set b i '.'
        done;
        let dotted_p = Bytes.unsafe_to_string b in
        of_string dotted_p

  let to_path n =
    let dir_sep = Filename.dir_sep.[0] in
    let b = Bytes.of_string n in
    for i = 0 to String.length n - 1 do
      if Bytes.get b i = '.' then Bytes.set b i dir_sep;
    done;
    Bytes.unsafe_to_string b

  let uniquify = Misc.Stdlib.String.uniquify

  module T = String
  let equal = T.equal
  let compare = T.compare

  module Set = struct
    include Set.Make (T)
    let to_string_set = Fun.id
  end

  module Map = Map.Make (T)
end

type t = { name : Name.t; dir : Misc.filepath; }
let name l = l.name
let dir l = l.dir

(* The following should not end up being repeatedly called. So we don't cache
   otherwise we could have them as lazy fields in [t] or, more generally,
   a file extension map of the library directory files. *)

let get_file l f =
  let file = Filename.concat l.dir f in
  match Sys.file_exists file with
  | false -> Error (strf "Library '%s': no %s file." l.name file)
  | true ->
      (* We make sure the returned file path does not get reinterpreted at
         some point by the [Load_path] machinery. This could happen if there
         are relative paths in OCAMLPATH. *)
      if Filename.is_implicit file
      then Ok Filename.(concat current_dir_name file)
      else Ok file

let cma l = get_file l "lib.cma"
let cmxa l = get_file l "lib.cmxa"
let cmxs l = get_file l "lib.cmxs"

(* Resolver *)

module Resolver = struct
  type lib = t
  type t = { ocamlpath : Ocamlpath.t; mutable libs : lib Name.Map.t; }

  let create ~ocamlpath = { ocamlpath; libs = Name.Map.empty }
  let ocamlpath r = r.ocamlpath
  let find r name = match Name.Map.find_opt name r.libs with
    | Some _ as v -> v
    | None ->
        let rec loop name_path = function
          | [] -> None
          | d :: ds ->
              let dir = Filename.concat d name_path in
              match Sys.file_exists dir with
              | false -> loop name_path ds
              | true ->
                  let lib = { name; dir } in
                  r.libs <- Name.Map.add name lib r.libs;
                  Some lib
        in
        loop (Name.to_path name) (Ocamlpath.to_dirs r.ocamlpath)

  let get r name = match find r name with
    | Some l -> Ok l
    | None ->
        Error (strf "Library '%s' not found in OCAMLPATH:\n %s"
                 (Name.to_string name)
                 (String.concat "\n " (Ocamlpath.to_dirs r.ocamlpath)))

  let err_in_archive ~file e =
    strf "Library resolution error in archive %s:\n%s" file e
end
