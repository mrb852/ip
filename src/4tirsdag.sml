(* TIRSDAG *)
datatype bentSpil = BentSlutter of int | BentSpiller of ingeSpil list
     and ingeSpil = IngeSlutter of int | IngeSpiller of bentSpil list

val bent7 = BentSpiller [ IngeSlutter 2,
                          IngeSpiller [ BentSlutter 0 ],
                          IngeSpiller [ BentSlutter 1, BentSlutter 0 ] ]

(* OPG A1 *)
(* Tæller antallet af gange som bent kan slutte spillet *)
(* bentSpillerSpil : bentSpil -> int *)
fun bentSlutterSpil (BentSlutter n)       = 1
  | bentSlutterSpil (BentSpiller [])      = raise Domain
  | bentSlutterSpil (BentSpiller [i])     = ingesTur i
  | bentSlutterSpil (BentSpiller (i::is)) = ingesTur i
                                            + bentSlutterSpil (BentSpiller is)

and ingesTur (IngeSlutter n)       = 0
  | ingesTur (IngeSpiller [])      = raise Domain
  | ingesTur (IngeSpiller [b])     = bentSlutterSpil b
  | ingesTur (IngeSpiller (b::bs)) = bentSlutterSpil b
                                     + ingesTur(IngeSpiller bs);
(* A3 *)
(* Tager en int og giver alle mulige spil, hvor Inge starter *)
(* startInge : int -> ingeSpil *)

fun startInge 0 = IngeSlutter 0
  | startInge 1 = IngeSlutter 1
  | startInge 2 = IngeSlutter 2
  | startInge 3 = IngeSpiller [BentSlutter 0]
  | startInge 4 = IngeSpiller [BentSlutter 1, BentSlutter 0]
  | startInge n =
    IngeSpiller [startBent (n-3), startBent (n-4), startBent (n-5)]

and startBent 0 = BentSlutter 0
  | startBent 1 = BentSlutter 1
  | startBent 2 = BentSlutter 2
  | startBent 3 = BentSpiller [IngeSlutter 0]
  | startBent 4 = BentSpiller [IngeSlutter 1, IngeSlutter 0]
  | startBent n =
    BentSpiller [startInge (n-3), startInge (n-4), startInge (n-5)]

fun maksimerInge ( IngeSlutter 0 ) = 0
  | maksimerInge ( IngeSlutter 1 ) = 1
  | maksimerInge ( IngeSlutter 2 ) = 2
  | maksimerInge ( IngeSlutter _ ) = raise Fail "Ugyldigt spil!"
  | maksimerInge ( IngeSpiller [] ) = raise Domain
  | maksimerInge ( IngeSpiller [ bs ] ) = minimerBent bs
(*
  | maksimerInge ( IngeSpiller [ bs1, bs2 ] ) =
      Int.max (minimerBent bs1, minimerBent bs2)
*)
  | maksimerInge ( IngeSpiller (bs0::bss) ) =
    Int.max (
       minimerBent bs0,
       maksimerInge ( IngeSpiller bss ) )

and minimerBent ( BentSlutter n ) = n
  | minimerBent ( BentSpiller [ is ] ) = maksimerInge is
  | minimerBent ( BentSpiller (is0::iss) ) =
    Int.min (
       maksimerInge is0,
       minimerBent ( BentSpiller iss ) )
  
and minimerBentMange [bs] = minimerBent bs
  | minimerBentMange (bs0::bss) = 
    Int.max (
       maksimerInge is0,
       minimerBent ( BentSpiller iss ) )

val _ = maksimerInge : ingeSpil -> int


(* Alternativ *)
