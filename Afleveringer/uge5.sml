val ll = [2, 4, 7, 11, 5, 99, ~17, 0, 7, 4, 2, 4, 7, 11];

fun qsort [] = []
  | qsort [x] = [x]
  | qsort (x :: xs) =
    let
      fun split [] = ([], [])
         | split(y :: ys) =
            let val (lt, ge) = split(ys)
            in
              if y < x then (y :: lt, ge)
              else (lt, y :: ge)
            end
      val (lt, ge) = split (xs)
    in
      qsort lt @ [x] @ qsort ge
    end;

fun psort f b [] = b
  | psort f b (x::xs) = psort f (f(x, b)) xs;