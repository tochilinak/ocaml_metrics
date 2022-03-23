module type GENERAL = sig
  type input = Tast_iterator.iterator

  val metric_id : string
  val run : Compile_common.info -> input -> input
  val get_result : unit -> (string * float) list
  val reset : unit -> unit
end
