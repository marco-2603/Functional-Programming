(* Es 5.1 - Potenza 1000 con let *)
fun thousandthPower (x:real)= 
let
  val a=x*x*x*x*x*x*x*x*x*x;
  val b=a*a*a*a*a*a*a*a*a*a;
  val c=b*b*b*b*b*b*b*b*b*b;
in
  c
end;

(* Es 5.2 - Split lista *)
fun split(nil) = (nil,nil)
  | split([a]) = ([a],nil)
  | split (a::b::cs) =
  let
    val coda = split (cs);
  in
    (a::(#1 coda),b::(#2 coda))
  end;
(* split([1,2,3,4,5,6]); *)

(* Es 5.4 - Somma coppie *)
fun sumPairs []= (0,0)
  | sumPairs ((a,b)::xs)= 
  let
    val (A,B) = sumPairs (xs);
  in
    (A + a ,B + b )
  end;

(* Es 5.5 - Massimo con let *)
fun maxListReal([x:real]) = x
  | maxListReal(x::xs) =
  let
    val maxtail=maxListReal (xs);
  in
    if x>maxtail then x else maxtail
  end;

(* Es 5.6 - Doppia esponenziale *)
fun doubleExp (x:real, 0)= x
  | doubleExp (x,i) = 
  let
    val halfExp=doubleExp(x,i-1);
  in
    halfExp*halfExp
  end;

(* Es 5.7 - Somma elementi pari e dispari *)
fun sumList(lst) =
let
    fun wrap([], even, odd) = (even, odd)
      | wrap([x], even, odd) = (even + x, odd)
      | wrap(x::y::xs, even, odd) = wrap(xs, even + x, odd + y)
in
    wrap(lst, 0, 0)
end;
(* sumList([1,2,3,4]); *)

(* Es 5.8 - Print lista *)
fun printList(nil)=nil
  | printList(x::xs) = (print(Int.toString x ^ "\n"); printList (xs));

(* Es 5.9 - Combinazioni *)
fun comb (n,m)= 
let
  fun stampa (ris) = print("\n"^ Int.toString n ^ "\n"^Int.toString m ^ "\n"^Int.toString ris)
in
  stampa(fact(n) div (fact(m)*fact(n-m)))
end;

(* Es 5.10 - Print X *)
fun powInt (_, 0) = 1  
  | powInt (x, n) = x * powInt(x, n-1);

fun printXs n = 
let
  fun oprint (num) = if num<=0 then () else (print("X");oprint(num-1))
in
  oprint(powInt(2,n))
end;
