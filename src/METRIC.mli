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
end

module type FUNCTION = sig
  include GENERAL

  val inner_reset : unit -> unit
end
