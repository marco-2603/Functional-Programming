(* Es 3.4 - Lunghezza di lista *)
fun len_rec(L:'a list, a:int) = if null L then a else len_rec(tl(L),a+1);
fun len (L:'a list) = len_rec(L,0);
(* val len = fn: 'a list -> int *)
(* len([1,2,3,4]); *)
(* val it = 4: int *)
