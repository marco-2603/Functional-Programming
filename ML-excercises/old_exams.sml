(* ESAME GIUGNO 2020 - Conta elementi unici *)
fun noduplicates [] = nil
    | noduplicates (x::xs) = 
    let 
        val filtra = List.filter (fn y => y<>x ) xs
    in 
        x::noduplicates filtra
    end;

fun lenght [] = 0
    | lenght (x::xs) = 1 + lenght(xs);

fun conta [] = 0
    | conta L = lenght(noduplicates(L));
(* conta ["red", "red", "green", "blue"]; *)

(* ESAME GIUGNO 2018 - Conta non duplicati *)
fun filtra [] = nil
  | filtra (x::xs) = 
  let
    val filtro = List.filter( fn a => a<>x) xs
  in  
    x::filtra filtro
  end;

fun conta2018 [] = 0
  | conta2018 (L) = lenght(filtra(L));
(* conta2018 ["pera", "pera", "pera", "pera"]; *)

(* ESAME GENNAIO 2018 - Lambda calcolo *)
datatype lambda_expr = Var of string
                     | Lambda of string * lambda_expr
                     | Apply of lambda_expr * lambda_expr;

val rec is_bound = fn s => fn Var v => s = v
                            | Lambda (v, e) => if (s = v) then true else is_bound s e
                            | Apply (e1, e2) => (is_bound s e1) orelse (is_bound s e2);

(* ESAME SETTEMBRE 2017 - Ambienti *)
datatype intonil = Nil | Int of int;
type ambiente = string -> intonil;
val ambientevuoto = fn _:string => Nil;

val lega = fn e:ambiente => fn nome => fn valore => 
    (fn n => if (n = nome) then (Int valore) else (e n)):ambiente;

(* Versione alternativa *)
fun legaAlt amb nome valore = 
    fn s => if s = nome then Int valore else amb s;

(* ESAME LUGLIO 2017 - Ciclo FOR *)
datatype FOR = For of int * (int -> int);
val rec eval = fn For (n, f) => fn x => 
    if (n > 1) then eval (For (n - 1, f)) (f x) else x;

(* ESAME GIUGNO 2017 - Somma elementi in posizioni pari *)
fun sommali z [] = z
  | sommali z (a::[]) = z
  | sommali z (a::b::[]) = z
  | sommali z (a::b::c::d) = c + (sommali z d);

(* ESAME BINARIO 2024 - Addizione binaria *)
fun reverse [] = []
  | reverse (x::xs) = reverse xs @ [x];

fun sum_binary ([], []) = []
  | sum_binary (xs, []) = xs
  | sum_binary ([], ys) = ys
  | sum_binary (xs, ys) =
    let 
        (* Funzione per fare l'addizione di due bit con riporto *)
        fun addBits (0, 0, 0) = (0, 0)  (* bit, carry *)
          | addBits (0, 0, 1) = (1, 0)
          | addBits (0, 1, 0) = (1, 0)
          | addBits (0, 1, 1) = (0, 1)
          | addBits (1, 0, 0) = (1, 0)
          | addBits (1, 0, 1) = (0, 1)
          | addBits (1, 1, 0) = (0, 1)
          | addBits (1, 1, 1) = (1, 1);
        
        (* Funzione principale che processa da destra a sinistra *)
        fun addWithCarry ([], [], 0) = []
          | addWithCarry ([], [], carry) = [carry]
          | addWithCarry ([], y::ys, carry) = 
              let val (bit, newCarry) = addBits (0, y, carry)
              in bit :: addWithCarry ([], ys, newCarry)
              end
          | addWithCarry (x::xs, [], carry) = 
              let val (bit, newCarry) = addBits (x, 0, carry)
              in bit :: addWithCarry (xs, [], newCarry)
              end
          | addWithCarry (x::xs, y::ys, carry) = 
              let val (bit, newCarry) = addBits (x, y, carry)
              in bit :: addWithCarry (xs, ys, newCarry)
              end;
              
    in 
        reverse (addWithCarry (reverse xs, reverse ys, 0))
    end;
(* val test6 = sum_binary ([1,0,1,1], [1,1,1]); (* [1,0,0,1,0] *) *)

(* ESERCIZIO SUFFIXES - Genera tutti i suffissi di una stringa *)
fun suffixes s =
    let 
        fun helper [] acc = acc
          | helper (x::xs) acc = 
              let val currentSuffix = implode (x::xs)
              in helper xs (currentSuffix :: acc)
              end
    in 
        reverse (helper (explode s) [])
    end;

(* Versione alternativa *)
fun suffixesAlt s =
    if s = "" then []
    else
        let
            val reversed = reverse s
            fun gen_suffixes [] = []
              | gen_suffixes (c::cs) = 
                  let
                      val suffix = implode (c::cs)
                  in
                      suffix :: gen_suffixes cs
                  end;
            val reversed_suffixes = gen_suffixes (explode reversed)
        in
            map reverse reversed_suffixes
        end;

(* ESAME - Togli lettera *)
fun togliLettera (nil) n = nil 
  | togliLettera (a::l) n = 
      if (length (explode a) <= n) 
      then 
         a::(togliLettera (l) n) 
      else 
         implode(tl(explode(a)))::(togliLettera (l) n);
