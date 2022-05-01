open Base
module Format = Caml.Format
open Zanuda_core
open Zanuda_core.Utils

type context =
  { mutable result : int
  ; notes : string Queue.t
  }

let ctx = { result = 0; notes = Queue.create () }
let metrics_group_id = "(test)"
let before_function _ = ()
let get_function_metrics_result () = []
let get_function_extra_info () = []
let get_module_extra_info () = Queue.to_list ctx.notes

let before_module () =
  ctx.result <- 0;
  Queue.clear ctx.notes
;;

let update () = ctx.result <- ctx.result + 1
let get_module_metrics_result () = [ "func-count", Int_result ctx.result ]

let run _ _ fallback =
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
                "Function #%d position: %s\nFunction #%d type: %s"
                ctx.result
                (location_str loc)
                ctx.result
                type_str
            in
            Queue.enqueue ctx.notes msg)
          ();
        fallback.expr self expr)
  }
;;
