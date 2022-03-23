open Base
open Caml.Format

type mode =
  | Unspecified
  | Dir of string

type t =
  { mutable outfile : string option
  ; mutable outgolint : string option
  ; mutable out_rdjsonl : string option
        (* Spec: https://github.com/reviewdog/reviewdog/tree/master/proto/rdf#rdjson *)
  ; mutable mode : mode
        (* Below options to manage file paths. Not sure are they really required *)
  ; mutable workspace : string option
  ; mutable prefix_to_cut : string option
  ; mutable prefix_to_add : string option
  ; mutable extra_includes : string list
  ; mutable verbose : bool
  }

let opts =
  { outfile = None
  ; outgolint = None
  ; out_rdjsonl = None
  ; mode = Unspecified
  ; workspace = None
  ; prefix_to_cut = Some "_build/default/"
  ; prefix_to_add = None
  ; extra_includes = []
  ; verbose = false
  }
;;

let mode () = opts.mode
let set_mode m = opts.mode <- m
let set_in_dir s = set_mode (Dir s)
let add_include s = opts.extra_includes <- s :: opts.extra_includes
let set_out_file s = opts.outfile <- Some s
let set_out_golint s = opts.outgolint <- Some s
let set_out_rdjsonl s = opts.out_rdjsonl <- Some s
let set_workspace s = opts.workspace <- Some s
let set_prefix_to_cut s = opts.prefix_to_cut <- Some s
let set_prefix_to_add s = opts.prefix_to_add <- Some s
let includes () = opts.extra_includes
let prefix_to_cut () = opts.prefix_to_cut
let prefix_to_add () = opts.prefix_to_add
let outfile () = opts.outfile
let out_golint () = opts.outgolint
let out_rdjsonl () = opts.out_rdjsonl
let verbose () = opts.verbose
let set_verbose () = opts.verbose <- true

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
    [ "-o", Arg.String set_out_file, "Set Markdown output file"
    ; "-ogolint", Arg.String set_out_golint, "Set output file in golint format"
    ; "-ordjsonl", Arg.String set_out_rdjsonl, "Set output file in rdjsonl format"
    ; "-ws", Arg.String set_workspace, "Set dune workspace root"
    ; "-del-prefix", Arg.String set_prefix_to_cut, "Set prefix to cut from file names"
    ; "-add-prefix", Arg.String set_prefix_to_add, "Set prefix to reprend to file names"
    ; "-I", Arg.String add_include, "Add extra include path for type checking"
    ; "-v", Arg.Unit set_verbose, "More verbose output"
    ]
    set_in_dir
    "Set root directory of dune project"
;;
