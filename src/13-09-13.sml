
(* Konverterer en samling mønter til et ørebeløb.                  *)
(* T = 20 kr = 2000 øre, t = 10 kr = 1000 øre, 5 = 5 kr = 500 øre, *)
(* 2 =  2 kr =  200 øre, 1 =  1 kr =  100 øre, h =         50 øre. *)

local
  (* finder øreværdi af en enkelt mønt *)
  fun value1 #"T" = 2000
    | value1 #"t" = 1000
    | value1 #"5" =  500
    | value1 #"2" =  200
    | value1 #"1" =  100
    | value1 #"h" =   50
    | value1 coin = raise Fail ("No coin named " ^ String.str coin)
in
  fun value "" = 0
    | value coins =
        value1 (String.sub (coins,0)) +
        value (String.extract (coins,1,NONE))
end

val testValue_T = value "T" = 2000
val testValue_t = value "t" = 1000
val testValue_5 = value "5" = 500
val testValue_2 = value "2" = 200
val testValue_1 = value "1" = 100
val testValue_h = value "h" = 50
val testValue_fejl = (if value "K" = 0 then false else false)
                     handle Fail _ => true
                         | _ => false
val testValue_multi = value "T52Tt1h" = 5850
val testValue_tom = value "" = 0



fun minCoins v =
      let
        val ore = v mod 100
        fun help v = if v >= 2000 then "T" ^ help (v-2000)
                     else if v >= 1000 then "t" ^ help (v-1000)
                     else if v >= 500 then "5" ^ help (v-500)
                     else if v >= 200 then "2" ^ help (v-200)
                     else if v >= 100 then "1" ^ help (v-100)
                     else if v >= 50 then "h" ^ help (v-50)
                     else ""
      in
        if ore < 25 then help (v - ore)
        else if ore < 75 then help (v - ore + 50)
        else help (v - ore + 100)
      end

val testCoins_0 = minCoins 0 = ""
