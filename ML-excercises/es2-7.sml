(* Es 2.7 - Arrotondamento a decimi *)
fun rnd (a:real) = real(floor(a*10.0))/10.0;
(* val rnd = fn: real -> real *)
(* rnd 10.123; *)
(* val it = 10.1: real *)
