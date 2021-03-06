open Base
open Caml.Format

type mode =
  | Unspecified
  | Dir of string

type t =
  { mutable mode : mode
        (* Below options to manage file paths. Not sure are they really required *)
  ; mutable workspace : string option
  ; mutable prefix_to_cut : string option
  ; mutable prefix_to_add : string option
  ; mutable extra_includes : string list
  ; mutable verbose : bool
  ; mutable verbose_list : string list option
  ; mutable metrics_list : string list option
  ; mutable sections_to_analyse : string list option
  }

let opts =
  { mode = Unspecified
  ; workspace = None
  ; prefix_to_cut = Some "_build/default/"
  ; prefix_to_add = None
  ; extra_includes = []
  ; verbose = false
  ; verbose_list = None
  ; metrics_list = None
  ; sections_to_analyse = None
  }
;;

let list_from_string str = String.split ~on:',' str
let mode () = opts.mode
let set_mode m = opts.mode <- m
let set_in_dir s = set_mode (Dir s)
let add_include s = opts.extra_includes <- s :: opts.extra_includes
let set_workspace s = opts.workspace <- Some s
let set_prefix_to_cut s = opts.prefix_to_cut <- Some s
let set_prefix_to_add s = opts.prefix_to_add <- Some s
let includes () = opts.extra_includes
let prefix_to_cut () = opts.prefix_to_cut
let prefix_to_add () = opts.prefix_to_add
let verbose () = opts.verbose
let set_verbose () = opts.verbose <- true
let verbose_list () = opts.verbose_list
let sections_to_analyse () = opts.sections_to_analyse
let set_sections_to_analyse str = opts.sections_to_analyse <- Some (list_from_string str)

let set_verbose_list str =
  set_verbose ();
  opts.verbose_list <- Some (list_from_string str)
;;

let metrics_list () = opts.metrics_list
let set_metrics_list str = opts.metrics_list <- Some (list_from_string str)

let recover_filepath filepath =
  let filepath =
    match prefix_to_cut () with
    | Some prefix when String.is_prefix filepath ~prefix ->
      String.drop_prefix filepath (String.length prefix)
    | Some prefix when verbose () ->
      Caml.Format.eprintf "Can't cut prefix '%s' from '%s'\n%!" prefix filepath;
      filepath
    | Some _ | None -> filepath
  in
  let filepath =
    match prefix_to_add () with
    | Some s -> sprintf "%s%s" s filepath
    | None -> filepath
  in
  filepath
;;

let parse_args () =
  let open Caml in
  Arg.parse
    [ "-ws", Arg.String set_workspace, "Set dune workspace root"
    ; "-del-prefix", Arg.String set_prefix_to_cut, "Set prefix to cut from file names"
    ; "-add-prefix", Arg.String set_prefix_to_add, "Set prefix to reprend to file names"
    ; "-I", Arg.String add_include, "Add extra include path for type checking"
    ; "-v", Arg.Unit set_verbose, "More verbose output"
    ; ( "-v-list"
      , Arg.String set_verbose_list
      , "List of metrics classes with verbose output. Example: -v-list \
         Halstead,cohesion,lines_of_code" )
    ; ( "-sec-list"
      , Arg.String set_sections_to_analyse
      , "List of dune executables and libraries to analyse. Example: -sec-list \
         main,utils_lib,graph_lib" )
    ; ( "-met-list"
      , Arg.String set_metrics_list
      , "List of metrics to show. Metrics is shown if it contains a substring from this \
         list. Example: -met-list Halstead,cohesion_LCOM_34,lines_of_code" )
    ]
    set_in_dir
    "Set root directory of dune project"
;;
