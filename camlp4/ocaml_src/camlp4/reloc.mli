(* camlp4r *)
(***********************************************************************)
(*                                                                     *)
(*                             Camlp4                                  *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

val zero_loc : Lexing.position
val shift_pos : int -> Lexing.position -> Lexing.position
val adjust_loc : Lexing.position -> MLast.loc -> MLast.loc
val linearize : MLast.loc -> MLast.loc
val patt :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.patt -> MLast.patt
val expr :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.expr -> MLast.expr
val module_type :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.module_type ->
    MLast.module_type
val sig_item :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.sig_item ->
    MLast.sig_item
val with_constr :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.with_constr ->
    MLast.with_constr
val module_expr :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.module_expr ->
    MLast.module_expr
val str_item :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.str_item ->
    MLast.str_item
val class_type :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_type ->
    MLast.class_type
val class_sig_item :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_sig_item ->
    MLast.class_sig_item
val class_expr :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_expr ->
    MLast.class_expr
val class_str_item :
  (MLast.loc -> MLast.loc) -> Lexing.position -> MLast.class_str_item ->
    MLast.class_str_item
