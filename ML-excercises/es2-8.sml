(* Es 2.8 - Rimozione secondo elemento *)
fun rem (a) = hd(a)::tl(tl(a));
(* val rem = fn: 'a list -> 'a list *)
(* rem [1,2,3,4,5]; *)
(* val it = [1, 3, 4, 5]: int list *)
