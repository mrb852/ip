(* Gruppeaflevering 4, 26. sept. 2013
 * Københavns Universitet
 * Rotendahl,    Benjamin
 * Harder,       Jacob
 * Jacobsen,     Nicklas Warming
 * Enevoldsen,   Christian Hohlmann *)

(* Om denne opgave:
 * Denne fil starter med at definere tirsdagsopgaverne
 * for at komme direkte til gruppeopgaverne søg på:
 * 'Gruppeafleveringen'
 * Vi har også lavet opgaven i 'Inge og Bent' versionen
 * den findes til sidst i opgaven. Søg på:
 * 'Inge og Bent version' *)

(* Definition af minTree/maxTree datatyperne *)

datatype minTree = P of int | MIN of maxTree list
     and maxTree = Q of int | MAX of minTree list

(* dta : ('a -> 'b) -> 'a list -> 'b list )
 * Hjælpe funktion. 'dta' for 'do to all'
 * Tager en funktion, f, og en liste, l, og returnerer
 * en ny liste af samme længde som indeholder billedet
 * af l's elementer over f *)

fun dta f [] = []
  | dta f (x ::xs) = (f x) :: (dta f xs)

(* Tirsdag Opgaver *)

(* 4T1 *)

val trae =
    MIN [
      MAX [ P 3,
            MIN [ Q 7, Q 5 ] ],
      Q 9,
      MAX [ P 6 ] ]

(* 4T2 *)

(* chooseMin : int list -> int
 * Returnerer det mindste heltal i listen *)

fun chooseMin [] = raise Domain
  | chooseMin (n :: []) = n
  | chooseMin (n :: ns) = Int.min(n, chooseMin(ns))

val test_chooseMin_1 = 1 = chooseMin [ 5, 4, 3, 2, 1, 2, 3, 4, 6 ]
val test_chooseMin_2 = ~10 = chooseMin [ 5, 3, 2, 1, 2, 3, ~10 ]

(* chooseMax : int list -> int
 * Returnerer det største heltal i listen *)

fun chooseMax [] = raise Domain
  | chooseMax (n :: []) = n
  | chooseMax (n :: ns) = Int.max(n, chooseMax(ns))

val test_chooseMax_1 = 66 = chooseMax [ 5, 4, 3, 2, 1, 2, 3, 4, 66 ]
val test_chooseMax_2 = 5 = chooseMax [ 5, 3, 2, 1, 2, 3, ~10 ]

(* minMax : minTree -> int
 * maxMin : maxTree -> int *)

fun minMax ( MIN [] : minTree ) = raise Domain
  | minMax ( P n ) = n
  | minMax ( MIN ms ) = chooseMin (dta maxMin ms)
and maxMin ( MAX [] : maxTree ) = raise Domain
  | maxMin ( Q n ) = n
  | maxMin ( MAX ms ) = chooseMax (dta minMax ms)

val test_minMax_1 = 5 = minMax trae

(* 4T3 *)

(* makeMax : int -> maxTree
 * makeMin : int -> minTree *)

fun makeMax 0 = Q 0
  | makeMax 1 = Q 1
  | makeMax 2 = Q 2
  | makeMax 3 = MAX [ P 0 ]
  | makeMax 4 = MAX [ P 0, P 1 ]
  | makeMax n = MAX [ makeMin (n - 3),
                      makeMin (n - 4),
                      makeMin (n - 5) ]
and makeMin 0 = P 0
  | makeMin 1 = P 1
  | makeMin 2 = P 2
  | makeMin 3 = MIN [ Q 0 ]
  | makeMin 4 = MIN [ Q 0, Q 1 ]
  | makeMin n = MIN [ makeMax (n - 3),
                      makeMax (n - 4),
                      makeMax (n - 5) ]

(* 4T4 *)

val tree11 = makeMax 11
val max11  = maxMin tree11

(* Gruppeafleveringen *)

(* 4G1 *)

(* validMin : minTree -> bool
 * validMax : maxTree -> bool *)

fun validMin (MIN []) = false
  | validMin (P _) = true
  | validMin (MIN ms) = List.all validMax ms
and validMax (MAX []) = false
  | validMax (Q _) = true
  | validMax (MAX ms) = List.all validMin ms

val test_valid_min_1 =
    false = validMin (MIN [ MAX [ MIN [] ], MAX [] ])
val test_valid_min_2 =
    true = validMin (MIN [ MAX [ MIN [ Q 1 ] ], MAX [ P 2 ] ])

(* 4G2 *)

(* minToMax : minTree -> maxTree
 * maxToMin : maxTree -> minTree *)

fun minToMax (P n) = Q n
  | minToMax (MIN ms) = MAX (dta maxToMin ms)
and maxToMin (Q n) = P n
  | maxToMin (MAX ms) = MIN (dta minToMax ms)

;trae;

val antiTrae = minToMax trae

(* 4G3 *)

(* zeroToN : int -> int list
 * Hjælpefunktion.
 * Laver listen [ 0, 1, 2, ..., n ] *)

fun zeroToN 0 = [ 0 ]
  | zeroToN n = (zeroToN (n - 1)) @ [ n ]

(* Først definerer vi en funktion 'findMaxAlt' som
 * gør det rigtige, men netop på den måde som vi ikke
 * måtte gøre det på. Vi vil bruge den til at teste
 * med *)

(* findMaxAlt : int -> int *)

fun findMaxAlt n = maxMin (makeMax n)

(* Her danner vi en liste af de første 31 resultater af
 * findMaxAlt, startende fra 0. E.g:
 * findMaxAlt <= [ 0, 1, ..., 30 ] *)

val result_findMaxAlt_1 = dta findMaxAlt (zeroToN 30)

(* Nu definerer vi den rigtige 'findMax', som ikke
 * opbygger et spiltræ *)

(* findMax : int -> int *)

fun findMax 0 = 0
  | findMax 1 = 1
  | findMax 2 = 2
  | findMax n =
    case (n mod 8) of
        3 => 0
      | 4 => 1
      | 5 => 2
      | 6 => 2
      | 7 => 2
      | 0 => 0
      | 1 => 0
      | 2 => 0
      | _ => raise Fail "(n mod 8) > 7?"

(* Nu sammenlignes resultaterne *)

val result_findMax_1 = dta findMax (zeroToN 30)

val test_findMax_1 = result_findMax_1 = result_findMaxAlt_1

(* Inge og Bent version *)

(*

datatype bentSpil = BentSlutter of int | BentSpiller of ingeSpil list
     and ingeSpil = IngeSlutter of int | IngeSpiller of bentSpil list

(* træer *)

val inge3 = IngeSpiller [ BentSlutter 0 ]
val bent3 = BentSpiller [ IngeSlutter 0 ]
val inge4 = IngeSpiller [ BentSlutter 0, BentSlutter 1 ]
val bent4 = BentSpiller [ IngeSlutter 0, IngeSlutter 1 ]
val inge5 = IngeSpiller [ BentSlutter 2, BentSlutter 1, BentSlutter 0 ]
val bent5 = BentSpiller [ IngeSlutter 2, IngeSlutter 1, IngeSlutter 0 ]
val inge6 = IngeSpiller [ BentSlutter 1, (* fordi Inge tager 5 taendstikker *)
                          BentSlutter 2, (* fordi Inge tager 4 taendstikker *)
                          BentSpiller [ IngeSlutter 0 ] ] (* 3 taendstikker *)
val inge7 = IngeSpiller [ bent4, bent3, BentSlutter 2 ]
val bent7 = BentSpiller [ inge4, inge3, IngeSlutter 2 ]
val bent10 = BentSpiller [ inge7, inge6, inge5 ]

(* Øvelse inge8 *)

(* Opg. A1 *)


(* bentSlutterSpil : BentSpil -> int
 * Tæller antal måder Bent kan slutte *)

fun bentSlutterSpil (bSpil : bentSpil) =
    let
      fun bentsTur (BentSlutter _) = 1
        | bentsTur (BentSpiller [iSpil]) = ingesTur iSpil
        | bentsTur (BentSpiller (iSpil :: xs)) =
          (ingesTur iSpil) + (bentsTur (BentSpiller xs))
      and ingesTur (IngeSlutter _) = 0
        | ingesTur (IngeSpiller [bSpil]) = bentsTur bSpil
        | ingesTur (IngeSpiller (bSpil :: xs)) =
          (bentsTur bSpil) + (ingesTur (IngeSpiller xs))
    in
      bentsTur bSpil
    end

val test_bentSlutterSpil_1 = 6 = bentSlutterSpil bent10
val test_bentSlutterSpil_2 = 3 = bentSlutterSpil bent7

(* Opg. A2 *)

val inge8 = IngeSpiller [ bent5, bent4, bent3 ]

(* Opg. A3 *)

fun oneTwo 0 = []
  | oneTwo n = (oneTwo (n - 1)) @ [ n ]

fun startInge 0 = IngeSlutter 0
  | startInge 1 = IngeSlutter 1
  | startInge 2 = IngeSlutter 2
  | startInge 3 = IngeSpiller [ startBent 0 ]
  | startInge 4 = IngeSpiller [ startBent 0, startBent 1 ]
  | startInge n = IngeSpiller [ startBent (n-3),
                                startBent (n-4),
                                startBent (n-5) ]
and startBent 0 = BentSlutter 0
  | startBent 1 = BentSlutter 1
  | startBent 2 = BentSlutter 2
  | startBent 3 = BentSpiller [ startInge 0 ]
  | startBent 4 = BentSpiller [ startInge 0, startInge 1 ]
  | startBent n = BentSpiller [ startInge (n-3),
                                startInge (n-4),
                                startInge (n-5) ]

val test_startBent_10 =
    bentSlutterSpil (startBent 10) = bentSlutterSpil bent10

(* Opg. A5 *)

(* cheap version *)
(*
fun maksimerInge (IngeSlutter n) = n
  | maksimerInge (IngeSpiller b) = minimerBent (hd b)
and minimerBent (BentSlutter n) = n
  | minimerBent (BentSpiller i) = maksimerInge (List.last i)
*)

fun chooseMin [] = raise Domain
  | chooseMin (n :: []) = n
  | chooseMin (n :: ns) = Int.min(n, chooseMin(ns))

val test_chooseMin_1 = 1 = chooseMin [ 5, 4, 3, 2, 1, 2, 3, 4, 6 ]
val test_chooseMin_2 = ~10 = chooseMin [ 5, 3, 2, 1, 2, 3, ~10 ]

fun chooseMax [] = raise Domain
  | chooseMax (n :: []) = n
  | chooseMax (n :: ns) = Int.max(n, chooseMax(ns))

val test_chooseMax_1 = 66 = chooseMax [ 5, 4, 3, 2, 1, 2, 3, 4, 66 ]
val test_chooseMax_2 = 5 = chooseMax [ 5, 3, 2, 1, 2, 3, ~10 ]

fun maksimerInge (IngeSlutter n) = n
  | maksimerInge (IngeSpiller bs) =
    chooseMax (map minimerBent bs)
and minimerBent (BentSlutter n) = n
  | minimerBent (BentSpiller is) =
    chooseMin (map maksimerInge is)

val test_maksimerInge_9 = maksimerInge (startInge 9)
val test_minimerBent_10 = minimerBent (startBent 10)

(* 4G1 *)

fun gyldigtIngeSpil (IngeSlutter _) = true
  | gyldigtIngeSpil (IngeSpiller []) = false
  | gyldigtIngeSpil (IngeSpiller bs) =
    List.all (gyldigtBentSpil) bs
and gyldigtBentSpil (BentSlutter _) = true
  | gyldigtBentSpil (BentSpiller []) = false
  | gyldigtBentSpil (BentSpiller is) =
    List.all (gyldigtIngeSpil) is

val ugyldigtIngeSpil_1 = IngeSpiller []
val ugyldigtIngeSpil_2 = IngeSpiller [ BentSlutter 2,
                                       BentSpiller [] ]
val test_gyldigtIngeSpil_1 =
    false = gyldigtIngeSpil ugyldigtIngeSpil_1
val test_gyldigtIngeSpil_2 =
    false = gyldigtIngeSpil ugyldigtIngeSpil_2
val test_gyldigtIngeSpil_9 =
    true = gyldigtIngeSpil (startInge 9)

(* 4G2 *)

fun bentTilInge (BentSlutter n) =
    IngeSlutter n
  | bentTilInge (BentSpiller is) =
    IngeSpiller (map ingeTilBent is)
and ingeTilBent (IngeSlutter n) =
    BentSlutter n
  | ingeTilBent (IngeSpiller bs) =
    BentSpiller (map bentTilInge bs)

val inge6 = IngeSpiller [ BentSlutter 1,
                          BentSlutter 2,
                          BentSpiller [ IngeSlutter 0 ] ]
val bent6 = ingeTilBent inge6

val test_ingeTilBent_1 =
    bent6 = BentSpiller [ IngeSlutter 1, IngeSlutter 2,
                          IngeSpiller [ BentSlutter 0 ] ]

val test_bentTilInge_10 =
    bentTilInge (startBent 10) = startInge 10

(* 4G3 *)

fun findMax n =
    let fun ingeMax (IngeSlutter n) = n
          | ingeMax (IngeSpiller bs) =
            chooseMax (map bentMax bs)
        and bentMax (BentSlutter n) = n
          | bentMax (BentSpiller is) =
            chooseMax (map ingeMax is)
    in ingeMax (startInge n) end

val inge12 = startInge 12

val test_findMax =
    findMax 12

*)
