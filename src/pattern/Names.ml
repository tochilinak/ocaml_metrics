open Typedtree

let texp_name = function
  | Texp_ident _ -> "Texp_ident"
  | Texp_constant _ -> "Texp_constant"
  | Texp_let _ -> "Texp_let"
  | Texp_function _ -> "Texp_function"
  | Texp_apply _ -> "Texp_apply"
  | Texp_match _ -> "Texp_match"
  | Texp_try _ -> "Texp_try"
  | Texp_tuple _ -> "Texp_tuple"
  | Texp_construct _ -> "Texp_construct"
  | Texp_variant _ -> "Texp_variant"
  | Texp_record _ -> "Texp_record"
  | Texp_field _ -> "Texp_field"
  | Texp_setfield _ -> "Texp_setfield"
  | Texp_array _ -> "Texp_array"
  | Texp_ifthenelse _ -> "Texp_ifthenelse"
  | Texp_sequence _ -> "Texp_sequence"
  | Texp_while _ -> "Texp_while"
  | Texp_for _ -> "Texp_for"
  | Texp_send _ -> "Texp_send"
  | Texp_new _ -> "Texp_new"
  | Texp_instvar _ -> "Texp_instvar"
  | Texp_setinstvar _ -> "Texp_setinstvar"
  | Texp_override _ -> "Texp_override"
  | Texp_letmodule _ -> "Texp_letmodule"
  | Texp_letexception _ -> "Texp_letexception"
  | Texp_assert _ -> "Texp_assert"
  | Texp_lazy _ -> "Texp_lazy"
  | Texp_object _ -> "Texp_object"
  | Texp_pack _ -> "Texp_pack"
  | Texp_letop _ -> "Texp_letop"
  | Texp_unreachable -> "Texp_unreachable"
  | Texp_extension_constructor _ -> "Texp_extension_constructor"
  | Texp_open _ -> "Texp_open"
;;

[%%if ocaml_version < (4, 11, 0)]

let tpat_name : Typedtree.pattern_desc -> string = function
  | Tpat_any -> "Tpat_any"
  | Tpat_var _ -> "Tpat_var"
  | Tpat_constant _ -> "Tpat_constant"
  | Tpat_tuple _ -> "Tpat_tuple"
  | Tpat_construct _ -> "Tpat_construct"
  | Tpat_variant _ -> "Tpat_variant"
  | Tpat_record _ -> "Tpat_record"
  | Tpat_array _ -> "Tpat_array"
  | Tpat_alias _ -> "Tpat_alias"
  | Tpat_lazy _ -> "Tpat_lazy"
  | Tpat_exception _ -> "Tpat_exception"
  | Tpat_or _ -> "Tpat_or"
;;

[%%else]

let tpat_name : type k. k Typedtree.pattern_desc -> string = function
  | Tpat_any -> "Tpat_any"
  | Tpat_var _ -> "Tpat_var"
  | Tpat_constant _ -> "Tpat_constant"
  | Tpat_tuple _ -> "Tpat_tuple"
  | Tpat_construct _ -> "Tpat_construct"
  | Tpat_variant _ -> "Tpat_variant"
  | Tpat_record _ -> "Tpat_record"
  | Tpat_array _ -> "Tpat_array"
  | Tpat_alias _ -> "Tpat_alias"
  | Tpat_lazy _ -> "Tpat_lazy"
  | Tpat_value _ -> "Tpat_value"
  | Tpat_exception _ -> "Tpat_exception"
  | Tpat_or _ -> "Tpat_or"
;;

[%%endif]
