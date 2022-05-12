open Zanuda_core

module Item : sig
  type t =
    | Root
    | Executable of string * int (* name, id *)
    | Library of string * int (* name, id *)
    | File of string (* filename *)
    | Module of string * string (* filename, modname *)
    | Function of string * string * string (* filename, modname, func_name *)

  val construct_executable : string -> t
  val construct_library : string -> t
end

val add_executable : string -> Item.t (* exe_name -> Item.Executable *)
val add_library : string -> Item.t (* lib_name -> Item.Library *)
val add_file : Item.t -> string -> unit (* (Item.Executable/Library) -> file -> unit *)
val add_module : string -> string -> unit (* file -> modname -> unit *)
val add_function : string -> string -> string -> unit
(* file -> modname -> function -> unit*)

val add_module_result : string -> string -> string -> METRIC.metric_result -> unit
(* file -> modname -> metric_id -> result -> unit *)

val add_func_result : string -> string -> string -> string -> METRIC.metric_result -> unit
(* file -> modname -> function -> metric_id -> result -> unit *)

val add_extra_info_project : string list -> unit
(* extra_info -> unit *)

val add_extra_info_module : string -> string -> string list -> unit
(* file -> modname -> extra_info -> unit *)

val add_extra_info_func : string -> string -> string -> string list -> unit
(* file -> modname -> filename -> extra_info -> unit *)

val report : bool -> unit -> unit
(* is_verbose -> unit -> unit *)
