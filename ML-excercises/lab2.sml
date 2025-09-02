(* Es 2.1 - Terzo elemento di una lista *)
fun third L = hd(tl(tl(L)));
(* val third = fn: 'a list -> 'a *)
(* third [2,3,4]; *)
(* val it = 4: int *)

(* Es 2.2 - Reverse di tupla a 3 elementi *)
fun reverse (a,b,c) = (c,b,a);
(* val reverse = fn: 'a * 'b * 'c -> 'c * 'b * 'a *)
(* reverse (1,2,3); *)
(* val it = (3, 2, 1): int * int * int *)

(* Es 2.3 - Terzo carattere di una stringa *)
fun thirdchar (x:string) = str(hd(tl(tl(explode(x)))));
(* val thirdchar = fn: string -> string *)
(* thirdchar "abcd"; *)
(* val it = "c": string *)

(* Es 2.4 - Ciclo di lista *)
fun cycle (x:'a list) = tl(x)@[hd(x)];
(* val cycle = fn: 'a list -> 'a list *)
(* cycle [1,2,3,4]; *)
(* val it = [2, 3, 4, 1]: int list *)

(* Es 2.5 - Minimo e massimo di tre interi *)
fun min_max_pair (a,b,c) = 
    ( if a<b then (if a<c then a else c) else (if b<c then b else c) , 
      if a>b then (if a>c then a else c) else (if b>c then b else c) );
(* val min_max_pair = fn: int * int * int -> int * int *)
(* min_max_pair (1,2,3); *)
(* val it = (1, 3): int * int *)

(* Es 2.6 - Ordinamento di tre interi *)
fun min(a,b,c) = if a<b then (if a<c then a else c) else (if b<c then b else c);
fun max(a,b,c) = if a>b then (if a>c then a else c) else (if b>c then b else c);
fun middle(a,b,c)= a+b+c - min(a,b,c) - max(a,b,c);
fun sort (a,b,c) = [min(a,b,c), middle(a,b,c), max(a,b,c)];

(* Es 2.7 - Arrotondamento a decimi *)
fun rnd (a:real) = real(floor(a*10.0))/10.0;
(* val rnd = fn: real -> real *)
(* rnd 10.123; *)
(* val it = 10.1: real *)

(* Es 2.8 - Rimozione secondo elemento *)
fun rem (a) = hd(a)::tl(tl(a));
(* val rem = fn: 'a list -> 'a list *)
(* rem [1,2,3,4,5]; *)
(* val it = [1, 3, 4, 5]: int list *)
