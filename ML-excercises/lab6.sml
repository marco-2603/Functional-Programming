(* Es 6.1 - Eccezioni custom *)
exception TooShort of int list;

fun third (x::y::z::zs) = z
  | third (x) = raise TooShort (x);

fun takeThird (x) = third (x) 
handle
  TooShort (x) => (print ("List too short.\n It only contains ");
  print (Int.toString (length x));
  print (" elements.\n");
  ~1);

(* Es 6.2 - Fattoriale con eccezioni *)
exception Neg of int;

fun factorial (x) = 
let
   fun fact (0) = 1
    | fact (x) = if x<0 then raise Neg (x) else x*fact(x-1);
in
  fact(x)
end
handle
  Neg(x) => (print ("negative number: "^Int.toString x ^ "\n"); 0);

(* Es 6.3 - Tabulazione funzione *)
fun tabulate (start:real , increment:real , points:int , func:real->real) = 
let 
  fun stampa (start,func)= print(Real.toString(start) ^ " " ^ Real.toString(func(start)) ^ "\n" )
in
  stampa(start,func);
  if points<=1 then () else tabulate(start+increment,increment,points-1,func ) 
end;

(* Es 6.4-6.6 - Map, reduce, filter personalizzati *)
fun simpleMapPos (nil)=nil
  | simpleMapPos (x::xs)= (if x>0.0 then x else 0.0)::simpleMapPos(xs);

fun reduceMax (x)=
let
  fun side (a,nil)= a
   | side (a,x::xs)= side( (if x>a then x else a) , xs )
in
  side(0.0,x)
end;

fun filterPos (nil)=nil
  | filterPos (x::xs)= if x>0.0 then x::filterPos(xs) else filterPos(xs);
