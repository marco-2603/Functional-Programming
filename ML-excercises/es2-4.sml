(* Es 2.4 - Ciclo di lista *)
fun cycle (x:'a list) = tl(x)@[hd(x)];
(* val cycle = fn: 'a list -> 'a list *)
(* cycle [1,2,3,4]; *)
(* val it = [2, 3, 4, 1]: int list *)
