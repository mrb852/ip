fun altsum [] = 0
	|	altsum [x] = x
	|	altsum (x1::x2::xs) = x1 - x2 + altsum xs

val altsum_test_1 = altsum[1,2,3,4,5] = 1 - 2 + 3 - 4 + 5

fun altsum' [] = 0
	|	altsum' (x::xs) = x - altsum' xs

val altsum'_test_1 = altsum'[1, 2, 3, 4, 5] = 1 - 2 + 3 - 4 + 5
