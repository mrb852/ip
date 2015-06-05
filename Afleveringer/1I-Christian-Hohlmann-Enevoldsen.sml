(*
 * 1. Individuelle opgave
 * Skrevet af Christian Hohlmann Enevoldsen
 *
 * Tests inkluderet
**)

(* Opgave 1I1  *)

(* Beregner potens n af a for både positive og negative tal *)
fun powerRealInt(a, 0) = 1.0
  |	powerRealInt(a : real, n) = 
  	if n > 0 then 
  		a * powerRealInt(a, n-1)
  	else 
  		1.0 / ( powerRealInt(a, ~n) ) 


 (* Opgave 1I2 *)

(* Beregner den største fælles nævner *)
fun gcd(0, q) = q
  |	gcd(p, q) = gcd(q mod p, p);

(* Bestemmer om p og q er relative primtal. *)
fun relativePrimes(p, q) = gcd(p, q) = 1;

(* Opgave 1I3 *)

(* Hjælper. Beregner det næste indbyrdes primtal, hvor n < m *)
fun nextNotRelativePrimeIncr(n, m) = 
	if relativePrimes(n, m) then 
		nextNotRelativePrimeIncr(n, m+1) 
	else m;

(* Beregner det næste indbyrdes primtal for givet n, som er større end n *)
fun nextNotRelativePrime(n) = nextNotRelativePrimeIncr(n, n+1);

(* Tests *)
val TEST_1i1 = "powerRealInt(0.99, ~100) = " ^ Real.toString(powerRealInt(0.99, ~100));
val TEST_1i3 = "nextNotRelativePrime(119) = " ^ Int.toString(nextNotRelativePrime(119));