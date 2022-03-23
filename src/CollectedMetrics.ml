open Base
open Caml.Format
open Utils

let metric_notes : string Queue.t = Queue.create ()
let add_note = Queue.enqueue metric_notes
let metric_results : (string * string * float) Queue.t = Queue.create ()

let add_result metric_id filename res =
  Queue.enqueue metric_results (metric_id, filename, res)
;;

let report verbose () =
  Queue.iter metric_results ~f:(fun (metric_id, filename, res) ->
      Format.printf "%s:%s: %.2f\n" metric_id filename res);
  let report_notes () =
    Format.printf "\n";
    Queue.iter metric_notes ~f:(fun s -> Format.printf "%s\n" s)
  in
  if verbose () then report_notes ()
;;
