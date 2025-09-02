(* Es 7.1 - Apply lista funzioni *)
fun apply nil v = nil
  | apply (F::Fs) v = F(v)::(apply Fs v);
(* apply [fn x=>x*2, fn x => x*x*x] 4; *)
(* val it = [8, 64]: int list *)

(* Es 7.2 - Curry *)
fun curry F a b c = F(a,b,c);
(* val A = curry (fn (a,b,c)=>a*b*c); *)

(* Es 7.3 - Conversione a real *)
val toreal = map real;
(* toreal [1,2,3]; *)
(* val it = [1.0, 2.0, 3.0]: real list *)

(* Es 7.4 - AND logico *)
val andb = foldr (fn (base,x) => base andalso x) true;
(* andb [true,false,true]; *)
(* val it = false: bool *)

(* Es 7.5 - Implode *)
val implodeChars = foldr (fn (x,base) => str(x) ^ base) "";
(* val implodeChars = foldr (op ^) "" o (map str); *)

(* ALBERI BINARI *)
datatype 'a btree = Empty 
  | Node of 'a * 'a btree * 'a btree;

(* Es 7.6 - Post order *)
fun post (Empty) = nil
  | post (Node(a,left,right)) = post(left) @ post(right) @ [a];

(* Es 7.7 - In order *)
fun inorder (Empty) = nil
  | inorder (Node(a,left,right)) = inorder(left) @ [a] @ inorder(right);

(* Es 7.8-7.9 - Map tree *)
type ('a , 'b) mapTree = ('a * 'b) btree;

fun sum Empty = 0
  | sum (Node((d , r), left, right)) = r + sum(left) + sum(right);
