(*
 * 2. Individuelle Opgave
 * Christian Holmann Enevoldsen
 *)

(* Opgave 2I1 *)

(* Sammenligner order, hvor LESS < EQUAL < GREATER *)
fun orderCompare (n, m) =
	let
		(* Pakker typen order til en int *)
		fun wrap LESS = ~1
			|	wrap EQUAL = 0
			|	wrap GREATER = 1
	in
		Int.compare(wrap(n), wrap(m))
	end;

(* Opgave 2I2 *)

(* Beregner det største ciffer i n *)
fun maxCiffer n =
	let
		(* Beregner det største ciffer i et tal n rekursivt,
		 * ved at gemme det midlertidige største ciffer m *)
		fun helper(m, 0) = m
			|	helper(m, n) =
					if n < 0 then helper(m, ~n)
					else helper( Int.max(m, n mod 10), n div 10 )
	in
		helper(0, n)
	end;

(* Opgave 2I3 *)

(* Bestemmer om x er i intervallet [0; 33[ *)
fun intervalNy(x) = x >= 0 andalso x < 33;

(* Bør retunere det samme som intervalNy *)
fun interval(x) = if x < 0 then false else x < 33;

(* Opgave 2I4 *)

(* Beregner den endelige tværsum af et tal n *)
fun endeligTvaerSum(n) =
	let
		(* Beregner tværsummen af et tal n *)
		fun tvaerSum(m, 0) = m
			|	tvaerSum(m, n) =
					if n < 0 then raise Domain
					else tvaerSum(m + n mod 10, n div 10)
	in
		if n < 10 andalso n >= 0 then n (* Basis tilfælde *)
		else endeligTvaerSum( tvaerSum(0, n) )
	end;

(* Opgave 2I5 *)

(* Tests Opgave 2I1 *)
val equalGreaterThanLess = orderCompare(EQUAL, LESS) = GREATER;
val equalLessThanGreater = orderCompare(EQUAL, GREATER) = LESS;
val equalEqualsEqual = orderCompare(EQUAL, EQUAL) = EQUAL;

(* Test opgave 2I2 *)
val maxCiffer42is4 = maxCiffer(42) = 4;
val naxCifferNeg42is4 = maxCiffer(~42) = 4;

(* Test opgave 2I3 *)
val neg1_isNotInInterval = not(interval(~1));
val pos1_isIndeedInInterval = interval(1);
val pos34_isNotInInterval = not(interval(34));

val neg1_isNotInIntervalNy = not(intervalNy(~1));
val pos1_isIndeedInIntervalNy = intervalNy(1);
val pos34_isNotInIntervalNy = not(intervalNy(34));

val intervalBehavesLikeIntervalNy =
	(interval(~1) = intervalNy(~1)) andalso
	(interval(1) = intervalNy(1)) andalso
	(interval(34) = intervalNy(34));


(* Test opgave 2I4 *)
val endeligTvaerSum_54327_is_3 = endeligTvaerSum(54327) = 3;
val endeligTvaerSum_neg_54327_throws_domain = (endeligTvaerSum(~54327); false) handle Domain => true;
