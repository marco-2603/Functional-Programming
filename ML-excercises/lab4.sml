(* Es 4.1 - Flip elementi alternati *)
fun flip (nil) = nil
  | flip [x] = [x]
  | flip (x::y::xs) = y::x::flip(xs);
(* val flip = fn: 'a list -> 'a list *)
(* flip[1,2,3,4,5]; *)
(* val it = [2, 1, 4, 3, 5]: int list *)

(* Es 4.2 - Rimozione i-esimo elemento *)
fun remove ([] , i) = []
  | remove (x::xs, 1) = xs
  | remove (x::xs, i) = x::remove(xs,i-1);
(* remove([1,2,3],3); *)
(* val it = [1, 2]: int list *)

(* Es 4.3 - Quadrato usando pattern matching *)
fun square (0 : int) = 0
  | square (n : int) = ((n-1)*(n-1) + 2*n) - 1;
(* square(5); *)
(* val it = 25: int *)

(* Es 4.4 - Ordinamento coppie *)
fun flipPairs (nil) = nil
  | flipPairs ((x,y)::xs) = 
      if x<y then (x,y)::flipPairs(xs) else (y,x)::flipPairs(xs);
(* flipPairs [(1,2),(4,3)]; *)
(* val it = [(1, 2), (3, 4)]: (int * int) list *)

(* Es 4.5 - Check vocale *)
fun vowel nil = false
  | vowel (#"a"::xs) = true
  | vowel (#"e"::xs) = true
  | vowel (#"i"::xs) = true
  | vowel (#"o"::xs) = true
  | vowel (#"u"::xs) = true
  | vowel (a::xs) = false;
(* vowel[#"a",#"b"]; *)
(* val it = true: bool *)

(* Es 4.6 - Membership in set *)
fun member (_, []) = false
  | member (a, x::xs) = if (a=x) then true else member (a,xs);
(* member (2,[2,3,1]); *)
(* val it = true: bool *)

(* Es 4.8 - Insert in set *)
fun insert ([] , n) = [n]
  | insert (x::xs, n) = if x=n then x::xs else x::insert(xs,n);
(* insert ([3,4,5],3); *)
(* val it = [3, 4, 5]: int list *)

(* Es 4.9 - Insert all *)
fun insertAll (_ , nil) = nil
  | insertAll (n, xs::xss) = (n::xs)::insertAll(n , xss);
(* insertAll (1,[[2,3],[],[3]]); *)
(* val it = [[1, 2, 3], [1], [1, 3]]: int list list *)

(* Es 4.11 - Prodotto differenze *)
fun proDiff1 (_,nil)= 1.0
  | proDiff1 (a,b::bs) = (a-b) * proDiff1(a,bs);

fun proDiff (nil) = 1.0
  | proDiff (b::bs) = proDiff1(b,bs) * proDiff(bs);

(* Es 4.12 - Pattern matching con case *)
val is_one = fn n => case n of
    1 => "is one"
  | _ => "anything else";
(* is_one 1; *)
(* val it = "is one": string *)
