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

open Clflags

let usage = "Usage: ocamlc <options> <files>\nOptions are:"

module Options = Main_args.Make_bytecomp_options (Main_args.Default.Main)

let main argv ppf =
  Clflags.add_arguments __LOC__ Options.list;
  Clflags.add_arguments __LOC__
    ["-depend", Arg.Unit Makedepend.main_from_option,
     "<options> Compute dependencies (use 'ocamlc -depend -help' for details)"];
  match
    Compenv.readenv ppf Before_args;
    Clflags.parse_arguments argv Compenv.anonymous usage;
    Compmisc.read_clflags_from_env ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    begin try
      Compenv.process_deferred_actions
        (ppf,
         Compile.implementation,
         Compile.interface,
         ".cmo",
         ".cma");
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments usage;
        exit 2
      end
    end;
    Compenv.readenv ppf Before_link;
    if
      List.length
        (List.filter (fun x -> !x)
           [make_archive;make_package;Compenv.stop_early;output_c_object])
        > 1
    then begin
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None ->
          Compenv.fatal
            "Please specify at most one of -pack, -a, -c, -output-obj";
      | Some ((P.Parsing | P.Typing) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf Compenv.fatal
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -output-obj"
          (String.concat "|"
             (Clflags.Compiler_pass.available_pass_names ~native:false))
      | Some P.Scheduling -> assert false (* native only *)
    end;
    if !make_archive then begin
      Compmisc.init_path () ~libs:[];
      let err_context = "archive creation" in
      let requires = Compenv.get_lib_requires_fatal_on_file ~err_context in
      let objfiles = Compenv.get_objfiles ~with_ocamlparam:false in
      let out_file = Compenv.extract_output !output_name in
      Bytelibrarian.create_archive ~requires objfiles out_file;
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path () ~libs:[];
      let extracted_output = Compenv.extract_output !output_name in
      let revd = Compenv.get_objfiles ~with_ocamlparam:false in
      Compmisc.with_ppf_dump ~file_prefix:extracted_output (fun ppf_dump ->
        Bytepackager.package_files ~ppf_dump (Compmisc.initial_env ())
          revd (extracted_output));
      Warnings.check_fatal ();
    end
    else if not !Compenv.stop_early && !objfiles <> [] then begin
      let target =
        if !output_c_object && not !output_complete_executable then
          let s = Compenv.extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll
            || Filename.check_suffix s ".c")
          then s
          else
            Compenv.fatal
              (Printf.sprintf
                 "The extension of the output file must be .c, %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          Compenv.default_output !output_name
      in
      Compmisc.init_path () ~libs:[];
      Bytelink.link (Compenv.get_objfiles ~with_ocamlparam:true) target;
      Warnings.check_fatal ();
    end;
  with
  | exception (Compenv.Exit_compiler n) ->
    n
  | exception x ->
    Location.report_exception ppf x;
    2
  | () ->
    Profile.print Format.std_formatter !Clflags.profile_columns;
    0
