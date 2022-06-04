type function_name =
  { name_ident_list : Ident.t list
  ; name_string : string
  }

type function_info =
  { is_rec : bool
  ; name : function_name
  ; block : function_name list
  ; ind_inside_block : int
  ; filename : string
  ; in_module : string
  }

type module_info =
  { mod_name : string
  ; filename : string
  ; is_anonymous : bool
  ; scope : int option
  }

type function_sig_info = { fun_sig_name : Ident.t }

type module_sig_info =
  { mod_sig_name : string
  ; filename : string
  }

type metric_result =
  | Int_result of int
  | Float_result of float
  | Delayed_result of (metric_result option * bool) ref (* bool: won't be calculated *)

type 'result iterator_actions =
  { (* functions called by My_Tast_iterator *)
    begin_of_function : function_info -> unit
  ; end_of_function : function_info -> 'result
  ; begin_of_module : module_info -> unit
  ; end_of_module : module_info -> 'result
  ; begin_of_function_sig : function_sig_info -> unit
  ; end_of_function_sig : function_sig_info -> unit
  ; begin_of_module_sig : module_sig_info -> unit
  ; end_of_module_sig : module_sig_info -> unit
  }

let default_iterator_actions : type k. k -> k iterator_actions =
 fun default_result ->
  { begin_of_function = (fun _ -> ())
  ; end_of_function = (fun _ -> default_result)
  ; begin_of_module = (fun _ -> ())
  ; end_of_module = (fun _ -> default_result)
  ; begin_of_function_sig = (fun _ -> ())
  ; end_of_function_sig = (fun _ -> ())
  ; begin_of_module_sig = (fun _ -> ())
  ; end_of_module_sig = (fun _ -> ())
  }
;;

type 'result iterator_builder =
  { actions : 'result iterator_actions
  ; run :
      Compile_common.info * string option
      -> string array (* file content *)
      -> Tast_iterator.iterator
      -> Tast_iterator.iterator
  }

let default_iterator_builder : type k. k -> k iterator_builder =
 fun default_result ->
  { actions = default_iterator_actions default_result; run = (fun _ _ x -> x) }
;;

type metrics_group_iterator_builder =
  { (* result for cmt: (metrics_results, extra_info) *)
    cmt : ((string * metric_result) list * string list) iterator_builder
  ; cmti : unit iterator_builder
  ; collect_delayed_metrics : unit -> unit
  ; get_project_extra_info : unit -> string list
  }

let default_metrics_group_iterator_builder =
  { cmt = default_iterator_builder ([], [])
  ; cmti = default_iterator_builder ()
  ; collect_delayed_metrics = (fun _ -> ())
  ; get_project_extra_info = (fun _ -> [])
  }
;;

module type GROUP = sig
  val metrics_group_id : string
  val get_iterator_builder : unit -> metrics_group_iterator_builder
end
