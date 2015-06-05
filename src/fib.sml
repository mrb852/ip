fun fib n =
	let
		fun helper (0, a, b) = a
			|	helper (n, a, b) = helper (n-1, b, a+b)
	in
		helper(n, 0, 1)
	end;

val f40 = fib 40;
