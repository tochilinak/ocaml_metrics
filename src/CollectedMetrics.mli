open METRIC

val add_file : string -> unit (* file -> unit *)
val add_module : string -> string -> unit (* file -> modname -> unit *)
val add_function : string -> string -> string -> unit
  (* file -> modname -> function -> unit*)

val add_module_result
  : string -> string -> string -> METRIC.metric_result -> unit
  (* file -> modname -> metric_id -> result -> unit *)

val add_func_result
  : string -> string -> string -> string -> METRIC.metric_result -> unit
  (* file -> modname -> function -> metric_id -> result -> unit *)

val add_extra_info_project
  : string list -> unit
  (* extra_info -> unit *)

val add_extra_info_module
  : string -> string -> string list -> unit
  (* file -> modname -> extra_info -> unit *)

val add_extra_info_func
  : string -> string -> string -> string list -> unit
  (* file -> modname -> filename -> extra_info -> unit *)

module Printer : sig
  val report : bool -> unit -> unit
  (* is_verbose -> unit -> unit *)
end
