(* Es 4.1 - Flip elementi alternati *)
fun flip (nil) = nil
  | flip [x] = [x]
  | flip (x::y::xs) = y::x::flip(xs);
(* val flip = fn: 'a list -> 'a list *)
(* flip[1,2,3,4,5]; *)
(* val it = [2, 1, 4, 3, 5]: int list *)
