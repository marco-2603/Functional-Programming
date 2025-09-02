(* Es 3.6 - Massimo lessicografico in lista di stringhe *)
fun maxList [] = raise Fail "Empty list"
  | maxList [x] = x
  | maxList ((x:string)::y::xs) = maxList ((if x > y then x else y) :: xs);
(* maxList(["a","abc","ab"]); *)
