open METRIC

type params =
  { verbose_metrics : string list
  ; metrics_to_show : string list
  ; cmt_iter_action_list :
      (string * ((string * metric_result) list * string list) iterator_actions) list
  ; cmti_iter_action_list : unit iterator_actions list
  ; cmt_run_list :
      (Compile_common.info * string option
       -> string array (* file content *)
       -> Tast_iterator.iterator
       -> Tast_iterator.iterator)
      list
  ; cmti_run_list :
      (Compile_common.info * string option
       -> string array (* file content *)
       -> Tast_iterator.iterator
       -> Tast_iterator.iterator)
      list
  }

val get_typed_on_structure
  :  params
  -> Compile_common.info
  -> string option
  -> string
  -> string array
  -> Typedtree.structure
  -> unit

val get_typed_on_signature
  :  params
  -> Compile_common.info
  -> string option
  -> string
  -> string array
  -> Typedtree.signature
  -> unit
