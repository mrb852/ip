(* Bertram André Nicolas
 * Christian Hohlmann Enevoldsen
 * Karl Damgaard Asmussen 
 * Mikkel Aleksander Høgh Rasmusen
 *)

(* 1G1
 *
 * Vi skal lave en funktion `solve2' som kan udregne de to reelle rødder i et
 * polynomium på formen \( a x^2 + b x + c = 0 \:|\: a \not= 0 \) givet som 
 * argumenter a, b og c.
 * Løsningen i opgaveformuleringen har en trykfejl, den rette formel er givet
 * ved:
 * \[ x = \frac{ - b \pm \sqrt{ b^2 - 4 a c } }{ 2 a } \]
 *
 * Opgaveformuleringen siger desuden at vi ikke skal tage højde for tilfældet
 * hvor der ikke findes reelle løsninger.
 *
 * Vi antager at det heller aldrig er tilfældet at a = 0 .
 *)

fun sqrt_discrm (a, b, c) = Math.sqrt(b*b - 4.0*a*c) ;

fun solve2 (a, b, c) =
  ( ( ~b + sqrt_discrm(a,b,c) ) / (2.0*a) ,
    ( ~b - sqrt_discrm(a,b,c) ) / (2.0*a) ) ;

(*
 * Poly/ML 5.2 Release
 * > use "/home/mhd/src/IP/1G-BN-CE-KA-MR.sml";
 * val sqrt_discrm = fn : real * real * real -> Real.Math.real
 * val solve2 = fn : real * real * real -> real * real
 * val it = () : unit
 * > solve2(2.0,3.0,1.0);                                            (* 1G2 *)
 * val it = (~0.5, ~1.0) : real * real
 * > solve2(2.0,3.0,4.0);                                            (* 1G3 *)
 * val it = (~nan, ~nan) : real * real
 *
 * 1G2 er korrekt, verificeret med CAS
 *
 * 1G3 er korrekt:
 * a = 2, b = 3 og c = 4 resulterer i d < 0
 * med d < 0 er der ingen reelle løsninger.
 * nan er ikke et reelt tal. mosml smider fejl.
 *)

(* 1G4
 * 
 * Det er sådan at der er regneregler for potenser:
 * \[ a^{2 n} = (a^n)^2 \]
 *)

fun square a = a * a ;

fun mul (x, a) = x * a ;

fun powerNew (a, 0) = 1
  | powerNew (a, 1) = a
  | powerNew (a, n) =
    if n mod 2 = 0
       then square ( powerNew (a, n div 2) )  (* even *)
       else a * powerNew (a, n - 1) ;         (* odd  *)

(*  > powerNew (2, 21)                                                    odd
 * ~> 2 * powerNew(2, 20)                                                 even
 * ~> 2 * square( powerNew(2, 10) )                                       even
 * ~> 2 * square( square( powerNew(2, 5) ) )                              odd
 * ~> 2 * square( square( 2 * powerNew(2, 4) ) )                          even
 * ~> 2 * square( square( 2 * square( powerNew(2, 2) ) ) )                even
 * ~> 2 * square( square( 2 * square( square( powerNew(2, 1) ) ) ) )      1
 * ~> 2 * square( square( 2 * square( square( 2 ) ) ) ) )                 * 0
 * ~> 2 * square( square( 2 * square( 4 ) ) ) )                           * 1
 * ~> 2 * square( square( 2 * 16 ) ) )                                    * 2
 * ~> 2 * square( square( 32 ) ) )                                        * 3
 * ~> 2 * square( 1024 )                                                  * 4
 * ~> 2 * 1048576                                                         * 5
 * ~> 2097152                                                             * 6
 *)

(*
 * Poly/ML 5.2 Release
 * > use "/home/mhd/src/IP/1G-BN-CE-KA-MR.sml";
 * val sqrt_discrm = fn : real * real * real -> Real.Math.real
 * val solve2 = fn : real * real * real -> real * real
 * val square = fn : int -> int
 * val powerNew = fn : int * int -> int
 * val it = () : unit
 * > powerNew(2, 21);
 * val it = 2097152 : int
 *)

fun squareCount (a, c) = (a * a, c + 1)

fun mulCount (a, (aa, c) ) = (a * aa, c + 1)

fun powerCount (a, 0) = (1, 0)
  | powerCount (a, 1) = (a, 0)
  | powerCount (a, n) =
    if n mod 2 = 0
       then squareCount ( powerCount (a, n div 2) )    (*even*)
       else mulCount (a, powerCount (a, n - 1) )       (*odd*)
;

(*fun count (a, 0) = 0
 *   | count (a, 1) = 0
 *   | count (a, n) =
 *     if n mod 2 = 0
 *        then 1 + count(a, n div 2)
 *        else 1 + count(a, n - 1)
 * ;
 *)

(* fun powerCount2 (a, n) = (powerNew (a, n), count (a, n)) ; *)

(* powerCount(2,21)                               
 * mulCount(2,powerCount(2,20))
 * mulCount(2,squareCount(powerCount(2,10)))
 * mulCount(2,squareCount(squareCount(powerCount(2,5))))
 * mulCount(2,squareCount(squareCount(mulCount(powerCount(2,4)))))
 * mulCount(2,squareCount(squareCount(mulCount(squareCount(powerCount(2,2))))))
 * mulCount(2,squareCount(squareCount(mulCount(squareCount(squareCount(powerCount(2,1)))))))
 * mulCount(2,squareCount(squareCount(mulCount(squareCount(squareCount((2,0)))))))
 * mulCount(2,squareCount(squareCount(mulCount(squareCount((4,1))))))
 * mulCount(2,squareCount(squareCount(mulCount((16,2)))))
 * mulCount(2,squareCount(squareCount((32,3))))
 * mulCount(2,squareCount((1024,4)))
 * mulCount(2,(1048576,5))
 * (2097152,6)
 *)

(* Poly/ML 5.2 Release
 * > use "/home/mhd/src/IP/1G-BN-CE-KA-MR.sml";
 * val sqrt_discrm = fn : real * real * real -> Real.Math.real
 * val solve2 = fn : real * real * real -> real * real
 * val square = fn : int -> int
 * val mul = fn : int * int -> int
 * val powerNew = fn : int * int -> int
 * val mulCount = fn : int * (int * int) -> int * int
 * val powerCount = fn : int * int -> int * int
 * val squareCount = fn : int * int -> int * int
 * val it = () : unit
 * > powerCount(2, 21)
 * val it = (2097152, 6) : int * int
 *)
