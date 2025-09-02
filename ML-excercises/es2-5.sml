(* Es 2.5 - Minimo e massimo di tre interi *)
fun min_max_pair (a,b,c) = 
    ( if a<b then (if a<c then a else c) else (if b<c then b else c) , 
      if a>b then (if a>c then a else c) else (if b>c then b else c) );
(* val min_max_pair = fn: int * int * int -> int * int *)
(* min_max_pair (1,2,3); *)
(* val it = (1, 3): int * int *)
