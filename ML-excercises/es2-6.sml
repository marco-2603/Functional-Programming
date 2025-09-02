(* Es 2.6 - Ordinamento di tre interi *)
fun min(a,b,c) = if a<b then (if a<c then a else c) else (if b<c then b else c);
fun max(a,b,c) = if a>b then (if a>c then a else c) else (if b>c then b else c);
fun middle(a,b,c)= a+b+c - min(a,b,c) - max(a,b,c);
fun sort (a,b,c) = [min(a,b,c), middle(a,b,c), max(a,b,c)];
