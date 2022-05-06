open Utils

module type GROUP = sig
  val metrics_group_id : string

  val run
    :  Compile_common.info
    -> string array
    -> Tast_iterator.iterator
    -> Tast_iterator.iterator

  val get_module_metrics_result : unit -> (string * metric_result) list
  val get_function_metrics_result : unit -> (string * metric_result) list
  val get_module_extra_info : unit -> string list
  val get_function_extra_info : unit -> string list
  val before_module : Utils.module_info -> unit
  val before_function : Utils.function_info -> unit
  val collect_delayed_metrics : unit -> unit
  val get_project_extra_info : unit -> string list
end
