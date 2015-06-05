(* Christian Hohlmann Enevoldsen *)

(* Ass. 5I1 *)

(* Calculates the product of an int list *)
val product = List.foldr op* 1;

(* Ass. 5I2 *)

(* Computes a function f o f, n times, for n > 1,
 * computes itself for n = 1 and for n = 0 it returns the input *)
fun funPower (f, 0) = (fn x => x)
  | funPower (f, 1) = f
  | funPower (f, n) = f o funPower(f, n-1);

(* Ass. 5I3 *)
(* Determines a fix point for a function f used with x *)
fun fix f x = if (f x) = x then x else fix f (f x);

(* Ass. 5I4 *)
(* Approximates a precision of sqrt x depending on
* how many times the function is repeated recursivly *)
val goNearRoot = fn x => fn g => (g + (x/g))/2.0;

(* Ass. 5I5 *)
(* Finds the square root of x *)
fun sqrt x = fix (fn g => goNearRoot x g) x;

(* Ass. 5I6 *)
infix 9 %;
fun f % g = g o f

(* Test product *)
val testProduct1 = product [2, 5, 11] = 110;
val testProduct2 = product [] = 1;

(* Test funPower *)
val square = fn x => x*x;
val testFunPower1 = funPower( square, 0 ) 2 = 2;
val testFunPower2 = funPower( square, 1 ) 2 = 4;
val testFunPower3 = funPower( square, 2 ) 2 = 16;

(* Test fix *)
val testFix = fix (fn x => x div 2 + 3) 17 = 6;

(* Test root *)

(* The first call will be aprox. 6. we use that as "g" for the second call,
 * to see if the result will be lower,
 * which it should be, because the sqrt of 12.0 is aprox. 3.46
 *)
local val testRoot1 = goNearRoot 12.0 1.0;
in val testRoot2 = goNearRoot 12.0 testRoot1 < testRoot1 end;

(* Test sqrt *)
val testSqrt1 = sqrt 9.0 = 3.0;
val testSqrt2 = sqrt 12.0 < 3.465 andalso sqrt 12.0 > 3.463;

(* Test infix *)
local
  val f = fn x => x * x * x;
  val g = fn y => y + y;
  val h = f % g;
in
  val testH = h 2 = 16;
  (* Pga. h x = g (f x) -> h 2 = g( f 2 ) = g (2 * 2 * 2) = 8 + 8 *)
end;