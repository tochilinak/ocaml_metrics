val analyze_dir
  :  cmt:(string -> string -> Typedtree.structure -> unit)
  -> cmti:(string -> string -> Typedtree.signature -> unit)
  -> on_exe:(string -> unit)
  -> on_lib:(string -> unit)
  -> string
  -> unit
