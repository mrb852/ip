fun map f [] = []
	|	map f (x :: xs) = f x :: map f xs;

fun map' (f, []) = []
  | map' (f, x :: xs) = f x :: map'(f, xs);

fun power (x, 0) = 1
  | power (x, n) = x * power(x, n - 1);

fun power' x 0 = 1
  | power' x n = x * power' x (n -1)

val sum =  List.foldr (op +) 0;

val testMap = map Int.toString [4, 5, 6] = ["4", "5", "6"];
val testMap' = map' (Int.toString, [3, 6, 33]) = ["3", "6", "33"];
val sumTest = sum [1, 2, 3] = 6;
val squared = power' 2;
val twoSquared = squared 2;

fun plus m n = m + n;

plus 2 3 = 5;
