(* Es 3.1 - Fattoriale *)
fun fact (a:int) = if a=1 then 1 else a*fact(a-1);
(* val fact = fn: int -> int *)
(* fact(10); *)
(* val it = 3628800: int *)
