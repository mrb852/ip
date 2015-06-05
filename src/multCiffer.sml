(*fun opgave (a, b) = if not a then b else true;*)
fun opgave1 (a, b) = a orelse b;
val test1 = opgave1(false, true) = true;

(*
fun opgave2(n) = 
	if n mod 2 <> 0 then false (* NOT EVEN = FALSE*)
	else if n mod 3 <> 0 then false (*  *)
	else true;
*)

fun opgave2(n) = n mod 6 = 0;
opgave2(9);