(* Vender et tal, og ignorerer efterfølgende 0'er *)
fun vendTal(n) =
	let
		fun vendTal2(0, k) = k
		|	vendTal2(n, k) = vendTal2(n div 10, (k * 10) + n mod 10)
 	in
 		vendTal2(n, 0)
 	end;


(* Test 2G3 *)
val vendTalTest300Equal3 = vendTal(300) = 3;