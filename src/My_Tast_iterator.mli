open Tast_iterator

type iterator_context =
  { filename : string
  ; mutable cur_module : string
  ; actions : unit METRIC.iterator_actions
  ; mutable inside_module_binding : bool (* default: false *)
  ; mutable module_binding_name : string (* default: "" *)
  ; mutable in_root : bool (* default: true *)
  }

val make_iterator_context
  :  filename:string
  -> cur_module:string
  -> actions:unit METRIC.iterator_actions
  -> iterator_context

val my_iterator : iterator_context -> Tast_iterator.iterator
