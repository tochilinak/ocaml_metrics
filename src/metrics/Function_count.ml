open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type input = Tast_iterator.iterator

let metric_id = "function_count"
let result = ref 0
let reset () = result := 0
let update () = result := !result + 1
let get_result () = [ "", float_of_int !result ]

let run _ fallback =
  let pat =
    let open Tast_pattern in
    texp_function (first_case (case pat_type drop exp_type))
  in
  let open Tast_iterator in
  { fallback with
    expr =
      (fun self expr ->
        let open Typedtree in
        let loc = expr.exp_loc in
        Tast_pattern.parse
          pat
          loc
          ~on_error:(fun _desc () -> ())
          expr
          (fun x y () ->
            update ();
            let type_str =
              Format.asprintf
                "(<from> %a) -> (<to> %a)"
                Printtyp.type_expr
                x
                Printtyp.type_expr
                y
            in
            let msg =
              Format.asprintf
                "Function #%d position: %s\nFunction #%d type: %s\n"
                !result
                (location_str loc)
                !result
                type_str
            in
            CollectedMetrics.add_note msg)
          ();
        fallback.expr self expr)
  }
;;
