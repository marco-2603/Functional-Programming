(* Es 3.1 - Fattoriale *)
fun fact (a:int) = if a=1 then 1 else a*fact(a-1);
(* val fact = fn: int -> int *)
(* fact(10); *)
(* val it = 3628800: int *)

(* Es 3.2 - Ciclo di lista i volte *)
fun cyclei (i:int , L:'a list) = 
    if i=0 then L else cyclei(i-1, tl(L)@[hd(L)]);
(* val cyclei = fn: int * 'a list -> 'a list *)
(* cyclei(2,[1,2,3,4]); *)
(* val it = [3, 4, 1, 2]: int list *)

(* Es 3.4 - Lunghezza di lista *)
fun len_rec(L:'a list, a:int) = if null L then a else len_rec(tl(L),a+1);
fun len (L:'a list) = len_rec(L,0);
(* val len = fn: 'a list -> int *)
(* len([1,2,3,4]); *)
(* val it = 4: int *)

(* Es 3.5 - Potenza *)
fun pow (a: int, b: real) = if a=1 then b else b*pow(a-1,b);
(* val pow = fn: int * real -> real *)
(* pow(3,2.1); *)
(* val it = 9.261: real *)

(* Es 3.6 - Massimo lessicografico in lista di stringhe *)
fun maxList [] = raise Fail "Empty list"
  | maxList [x] = x
  | maxList ((x:string)::y::xs) = maxList ((if x > y then x else y) :: xs);
(* maxList(["a","abc","ab"]); *)
