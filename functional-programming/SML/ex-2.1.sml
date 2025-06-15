(* Write a function thirdthat computes the third element of a list (it doesn't have to work properly on shorter lists). *)

fun third L = hd(tl(tl(L)));
(* val third = fn: 'a list -> 'a *)

third [2,3,4];
(* val it = 4: int *)
