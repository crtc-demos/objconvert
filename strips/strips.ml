external ext_stripper :
  (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  bool -> int list list = "run_strips"

let run_strips inp degenerate_tristrips =
  let i32 = Array.map Int32.of_int inp in
  let arr = Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout i32 in
  ext_stripper arr degenerate_tristrips
