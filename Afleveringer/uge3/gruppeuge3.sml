(* returnerer en liste af strenge separeret af et bestemt tegn *)
fun splitAt(c, s) =
  let fun indexOfChar(c, s, i) =
    if i = size s then NONE
    else if String.sub(s, i) = c then SOME i
    else indexOfChar(c, s, i+1)
  in
    case indexOfChar(c, s, 0) of
      SOME i =>
        let val line = substring(s, 0, i)
            val rest = substring(s, i+1, size s-(i+1))
        in line::splitAt(c, rest)
        end
      | NONE => [s]
  end;

(* Test splitAt *)
val testSplitAtEmpty = splitAt(#".", "") = [""]
val testSplitAtHejDotMed = splitAt(#".", "hej.med") = ["hej", "med"]
val testSplitAtDotHejDotMed = splitAt(#".", ".hej.med") = ["","hej", "med"]



