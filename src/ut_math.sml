load "Math";

(* 
	Udregner BMI-tal ud fra vægt og en højde 
*)
fun bmi(weight, height) = weight / ( height * height );

(* 
	Udregner fakultet af n >= 0 
*)
fun fact 0 = 1
| 	fact n = n * fact(n - 1);

(* Udregner potens af n *)
fun power (x, 0) = 1
|	power (x, n) = x * power(x, n - 1);

fun powerReal(x, 0.0) = 1.0
|	powerReal(x, n) : real = x * powerReal(x, n - 1.0);

(* Udregner arealet af en circel *)
fun circleArea r = Math.pi * r * r;

(* Udregner arealet af en ring *)
fun ringArea (outer, inner) = abs(circleArea(outer) - circleArea(inner));


(* Random assignments *)

fun fooint(n, k) = 2*n - k*k;

fun fooreal(n, k) : real = 2.0 * n - k * k; 

fun foomix(n, k) = 2.0 * n - real(k*k);

infixr 8 ** 

fun x **  0 = 1
	| x **  n =
			if n mod 2 = 0 then (x*x) ** (n div 2)
			else x ** (n-1)*x

val pow = 4 ** 3 ** 2;

infix 4 vec2Add ;
infix 7 vec2Dot;

fun (x1 : real, y1 : real) vec2Add (x2, y2) = (x1+x2, y1+y2);
fun (x1 : real, y1 : real ) vec2Dot (x2, y2) = x1*x2 +  y1+y2;
	
(2.0, 4.0) vec2Add (3.0, 1.1);
(2.32, 4.31) vec2Dot (3.3, 1.1);

infix 0 +=;

fun x += y = x + y;

val x = 1;
val y = 2;
x+=y;

