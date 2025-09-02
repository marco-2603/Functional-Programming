(* Es 3.5 - Potenza *)
fun pow (a: int, b: real) = if a=1 then b else b*pow(a-1,b);
(* val pow = fn: int * real -> real *)
(* pow(3,2.1); *)
(* val it = 9.261: real *)
