(* MODULI E SIGNATURE *)
signature SET =
sig
  type 'a set
  val emptyset : 'a set
  val isin : ''a -> ''a set -> bool
  val addin : ''a -> ''a set -> ''a set
  val removefrom : ''a -> ''a set -> ''a set
end;

structure Set =
struct
  type 'a set = 'a list;
  val emptyset = nil;
  
  fun isin _ nil = false
  | isin x (s::xs) = (x=s) orelse isin x xs;
  
  fun addin x S = if (isin x S) then S else x::S;
  
  fun removefrom _ nil = nil
  | removefrom x (s::xs) = if (x=s) then xs else s::removefrom x xs;
end:>SET;

(* SIGNATURE TREE *)
signature TREE =
sig
  datatype 'a T = Lf | Br of 'a * 'a T * 'a T;
  val countnodes : ''a T -> int
  val depth : ''a T -> int
  val mirror : ''a T -> ''a T
end;

structure Tree = 
struct
  datatype 'a T = Lf | Br of 'a * 'a T * 'a T;
  
  fun countnodes Lf = 0
    | countnodes (Br(a,lef,rig)) = 1 + countnodes lef + countnodes rig;
  
  fun depth Lf = 0
    | depth (Br(a,l,r)) = 1 + (if (depth l > depth r) then depth l else depth r);
  
  fun mirror Lf = Lf
    | mirror (Br(a,l,r)) = Br(a,mirror r, mirror l);
end:>TREE;

(* Es 8.9-8.10 - BST lookup e assign *)
fun lt (a,b) = if a<b then true else false;
exception Missing;

fun lookup lt Empty a = raise Missing
  | lookup lt (Node((d,c),l,r)) a = 
      if lt(a,c) then lookup lt l a 
      else if lt(c,a) then lookup lt r a 
      else c;

fun assign lt Empty a b = Node((a,b),Empty,Empty)
  | assign lt (Node((d,c),l,r)) a b = 
      if lt(a,c) then Node((d,c),assign lt l a b , r) 
      else if lt(c,a) then Node((d,c), l , assign lt r a b) 
      else Node((d,b),l,r);
