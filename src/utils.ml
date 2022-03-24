open Base
open Caml.Format

let printfn fmt = kfprintf (fun ppf -> fprintf ppf "\n%!") std_formatter fmt

module ErrorFormat = struct
  let pp ppf ~filename ~line ~col:_ msg x =
    fprintf ppf "%s:%d:%d:%a\n%!" filename line (* col *) 0 msg x
  ;;
end

type rdjsonl_code = string * string option

module RDJsonl : sig
  val pp
    :  formatter
    -> filename:string
    -> line:int
    -> ?code:rdjsonl_code
    -> (formatter -> 'a -> unit)
    -> 'a
    -> unit
end = struct
  let pp ppf ~filename ~line ?code msg x =
    let location file ~line ~col =
      `Assoc
        [ "path", `String file
        ; "range", `Assoc [ "start", `Assoc [ "line", `Int line; "column", `Int col ] ]
        ]
    in
    let j =
      `Assoc
        ([ "message", `String (asprintf "%a" msg x)
         ; "location", location filename ~line ~col:1
         ; "severity", `String "INFO"
         ]
        @
        match code with
        | None -> []
        | Some (desc, None) -> [ "code", `Assoc [ "value", `String desc ] ]
        | Some (desc, Some url) ->
          [ "code", `Assoc [ "value", `String desc; "url", `String url ] ])
    in
    fprintf ppf "%s\n%!" (Yojson.to_string j)
  ;;
  (* { "message": "Constructor 'XXX' has no documentation attribute",  "location": {    "path": "Lambda/lib/ast.mli",    "range": {      "start": { "line": 12, "column": 13 }, "end": { "line": 12, "column": 15      }    }  },  "severity": "INFO",  "code": {  "value": "RULE1",    "url": "https://example.com/url/to/super-lint/RULE1"  }}*)
end

let cut_build_dir s =
  let prefix = "_build/default/" in
  if String.is_prefix ~prefix s then String.drop_prefix s (String.length prefix) else s
;;

module Report = struct
  let txt ~loc ~filename ppf msg msg_arg =
    Option.iter !Location.input_lexbuf ~f:Lexing.flush_input;
    Location.input_name := cut_build_dir filename;
    let loc =
      let open Location in
      { loc with
        loc_start = { loc.loc_start with pos_fname = !input_name }
      ; loc_end = { loc.loc_end with pos_fname = !input_name }
      }
    in
    let main = Location.mkloc (fun ppf -> msg ppf msg_arg) loc in
    let r = Location.{ sub = []; main; kind = Report_alert "zanuda-linter" } in
    Location.print_report ppf r
  ;;

  let rdjsonl ~loc ~filename ~code ppf msg msg_arg =
    let code = code, Some "https://kakadu.github.io/zanuda/" in
    RDJsonl.pp ppf ~filename ~line:loc.Location.loc_start.pos_lnum ~code msg msg_arg
  ;;
end

let location_str loc =
  Location.print_loc Format.str_formatter loc;
  String.drop_prefix (String.drop_suffix (Format.flush_str_formatter ()) 4) 4
;;

let short_location_str loc =
  let open Location in
  let (_, l1, p1) = get_pos_info loc.loc_start in
  let (_, l2, p2) = get_pos_info loc.loc_end in
  Format.sprintf "%d:%d-%d:%d" l1 p1 l2 p2
;;

let empty_loc loc =
    let open Location in
    let (s1, l1, p1) = get_pos_info loc.loc_start in
    let (s2, l2, p2) = get_pos_info loc.loc_end in
    (String.equal s1 s2) && (l1 = l2) && (p1 = p2)
;;
