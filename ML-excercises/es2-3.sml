(* Es 2.3 - Terzo carattere di una stringa *)
fun thirdchar (x:string) = str(hd(tl(tl(explode(x)))));
(* val thirdchar = fn: string -> string *)
(* thirdchar "abcd"; *)
(* val it = "c": string *)
