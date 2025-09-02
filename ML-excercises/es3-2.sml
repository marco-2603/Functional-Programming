(* Es 3.2 - Ciclo di lista i volte *)
fun cyclei (i:int , L:'a list) = 
    if i=0 then L else cyclei(i-1, tl(L)@[hd(L)]);
(* val cyclei = fn: int * 'a list -> 'a list *)
(* cyclei(2,[1,2,3,4]); *)
(* val it = [3, 4, 1, 2]: int list *)
