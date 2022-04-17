module type GENERAL = sig
  val metric_id : string

  val run
    :  Compile_common.info
    -> string array
    -> Tast_iterator.iterator
    -> Tast_iterator.iterator

  val get_result : unit -> (string * float) list
  val reset : unit -> unit
  val extra_info : unit -> string list
  val before_function : Utils.function_info -> unit
end
