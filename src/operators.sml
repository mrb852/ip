(* Power *)
infixr 8 **

(* Logical *)
infix &&
fun x && y = x andalso y;

infix ||
fun x || y = x orelse y;

fun !x = not(x);

fun x **  0 = 1
	| x **  n =
			if n mod 2 = 0 then (x*x) ** (n div 2)
			else x ** (n-1)*x


datatype ('a, 'b) either = 
    THIS of 'a 
  | THAT of 'b;

val i = THAT 3;