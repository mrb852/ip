(* Christian Hohlmann Enevoldsen *)
(* IP Eksamen 2013. *)

load "Listsort";
datatype command = Turn | Move of int | Repeat of int * command list
type program     = command list
type point       = int * int
type drawing     = point list
datatype heading = North | South | East | West
type state       = point * heading


(* Opgave 1.a *)

(* Finder det første element i en liste som ikke er NONE *)
fun firstDefined[] = NONE
  | firstDefined(x::xs) = case x of
                            SOME x => SOME x
                          | NONE => firstDefined(xs)

(* Opgave 1.b *)

(* Omdanner en option list til en værdiliste, hvor NONE elementer efterlades *)
fun allDefined[] = []
  | allDefined(x::xs) = case x of
                          SOME x => valOf(SOME x)::allDefined(xs)
                        | NONE => allDefined(xs);


(* Opgave 2.a *)

(* Finder ud af om en streng er en delsekvens af en anden streng *)
fun subsequence(s, t) =
  let
    val cL = explode(s)
    val sL = explode(t)
    fun sub'([], _) = true
      | sub'(_, []) = false
      | sub'(c::cs, s::ss) = if c = s then sub'(cs, ss)
                             else sub'(c::cs, ss)
  in
    sub'(cL, sL)
  end

(* Opgave 3.a *)

(* Bestemmer om et tals cifre går op i tallet selv. *)
fun digitDivisible n =
  let
    (* Laver en liste med tallene *)
    fun getDigits(r, 0) = []
      | getDigits(r, m) = (m mod 10)::getDigits(r mod 10, m div 10)

    (* Bestemmer om alle tal går op i n *)
    fun digitDivisible'([]) = true
      | digitDivisible'(x::xs) = x > 0 andalso n mod x = 0
                                 andalso digitDivisible'(xs)
  in
    n > 0 andalso digitDivisible'(getDigits(n, n))
  end

(* Opgave 3.b *)

(* Laver en liste med alle tal som går op i m *)
fun digitDivisibleBetween(m, n) = if m > n then [] else
                                  if digitDivisible(m) then
                                  m::digitDivisibleBetween(m+1, n) else
                                  digitDivisibleBetween(m+1, n)

(* Opgave 4.a *)

(* Laver en nedtællingsliste fra n, hvor n >= 0 *)
fun countDown n = if n < 0 then raise Domain
                  else rev ( List.tabulate(n+1, fn n => n) );

(* Opgave 4.b *)

(* Bestemmer om en liste af heltal er sorteret i faldende orden *)
fun orderedList(xs) = (rev (Listsort.sort Int.compare xs)) = xs;

(* Opgave 4.c *)
fun nns n = if n < 0 then raise Domain else
            List.concat ( List.tabulate( n+1, fn x =>
                          List.tabulate( x,   fn y => x ) ) );

(* Opgave 5.a *)
open TextIO;
local

  (* Laver en SVG linjestreng  ud fra to punkter *)
  fun lineString((x1, y1), (x2, y2)) =
    "<line x1=\"" ^ Int.toString(x1) ^ "\" y1=\"" ^ Int.toString(y1) ^
       "\" x2=\"" ^ Int.toString(x2) ^ "\" y2=\"" ^ Int.toString(y2) ^ "\" />"

  (* Konverterer en drawing til svg linjer *)
  fun getSVGLines([]) = ""
    | getSVGLines([x]) = ""
    | getSVGLines((x1, y1)::(x2, y2)::xs) =
        lineString((x1, y1), (x2, y2)) ^ "\n" ^ getSVGLines((x2, y2)::xs)
in
  (* Skriver en SVG fil, som indeholder en tegning *)
  fun drawingToSVG file draw =
    let
      val os = TextIO.openOut file;
      val svgBodyStart = "<svg xmlns=\"http://www.w3.org/2000/svg\"style=\"stroke: black; stroke-width: 2px;\">"
      val svgBodyEnd = "</svg>"
    in
      TextIO.output(os, svgBodyStart ^ getSVGLines(draw) ^ svgBodyEnd)
      before TextIO.flushOut(os)
      before TextIO.closeOut(os)
    end
end

(* Opgave 6.a *)

val draw1 = [ Turn, Move 3, Turn, Move 2, Turn, Move ~1, Move 2]

(* Opgave 6.b*)
local

  (* Fjerner alle Turn kommando, hvor de fremkommer 4 gange i træk. *)
  fun removePivots([]) = []
    | removePivots([x]) = [x]
    | removePivots(x::xs) =
          if length(x::xs) < 4 then x::xs else
          if List.take(x::xs, 4) = [Turn, Turn, Turn, Turn] then
            removePivots(List.drop(x::xs, 4))
          else
            x::removePivots(xs)

  (* Gennemløber listen og tjekker om x og xs har samme fortegn, så de kan
     samles til en beregning. *)
  fun mergeMoves([]) = []
    | mergeMoves([x]) = [x]
    | mergeMoves(x::xs::xss) =
      let
        (* Tjekker om en kommando er Move kommandoen. *)
        fun isMove(Move x) = true
            | isMove(_) = false

        (* Returnerer værdien af en Move kommando. *)
        fun valOfMove(Move p) = p
          | valOfMove(_) = raise Fail "Not a Move command"
      in
        (* Tjekker først om begge kommando er af typen Move. *)
        (* Det afgør nemlig om der kommandoerne skal samles til en eller ej. *)
        if isMove(x) andalso isMove(xs) then
          let
            val vx = valOfMove(x)
            val vxs = valOfMove(xs)
          in
            if vx = 0 then mergeMoves(xs::xss) else

            (* Tjekker om de har samme fortegn *)
            if vx > 0 andalso vxs > 0 then Move(vx + vxs)::mergeMoves(xss)
            else if vx < 0 andalso vxs < 0 then Move(vx + vxs)::mergeMoves(xss)
            else x::mergeMoves(xs::xss)
          end
        else
          x::mergeMoves(xs::xss)
      end

    (* Fortsætter med at flette moves sammen indtil der ikke er forskel
       på den sidste og den næste. *)
    fun nestedMergeMoves(xs) =
      let
        val lastMerged = mergeMoves(xs)
      in
        if lastMerged = mergeMoves(lastMerged) then lastMerged
        else nestedMergeMoves(lastMerged)
      end
in

  (* Optimerer et program, så det bruger færre instruktioner *)
  fun optimise[] = []
    | optimise (xs) =
      let
        (* Tjekker om en repeat kommando er overflødig *)
        fun isEmptyRepeat(Repeat(0, x)) = true
          | isEmptyRepeat(_) = false

        (* Tjekker om en Move kommando er overflødig *)
        fun isValidMove(Move n) = not(n = 0)
          | isValidMove(_) = true
      in
        let

          (* Klargører optimeringen af programmet ved at smide alle
             overflødige repeat og Move kommandoer ud *)
          fun preOptimise([]) = []
            | preOptimise(x::xs) =
                  if isEmptyRepeat(x) orelse not(isValidMove(x)) then
                    preOptimise(xs)
                  else x::preOptimise(xs)

          (* Optimerer programmet ved at samle beregninger, og fjerne tilfælde
             hvor skildpadden ville dreje rundt om sig selv. *)
          fun optimise([]) = []
            | optimise(xs) = nestedMergeMoves(removePivots(xs))
        in
          optimise(preOptimise(xs))
        end
      end
end

(* Opgave 6.c *)

(* Konverterer alle repeat til normale kommandoer *)
fun removeRepeats([]) = []
  | removeRepeats(x::xs) =
    let

      (* Tjekker om et program har repeat kommandoer *)
      fun hasRepeats([]) = false
        | hasRepeats(x::xs) = case x of
          Repeat(n, x) => true
          | _ => hasRepeats(xs)

      (* Konverterer en repeat kommando om til et program *)
      fun convertRepeat(Repeat(0, x)) = []
        | convertRepeat(Repeat(n, x)) =
          x @ convertRepeat(Repeat(n-1, x))
        | convertRepeat(x) = [x]

      (* Programmet som rekursivt for fjernet sine "nested" repeats. *)
      val f = convertRepeat(x) @ removeRepeats(xs)
    in
      if hasRepeats(f) then removeRepeats(f) else f
    end

(* Opgave 6.d *)
local

  (* Drejer et kvarter med uret *)
  fun turnRight(h) = case h of
    North => East
    | East => South
    | South => West
    | West => North;

  (* Bevæger skildpadden *)
  fun move(h, (x, y), Move n) =
        case h of
          North => (x, y-n)
        | East => (x+n, y)
        | South => (x, y+n)
        | West => (x-n, y)

  (* Hjælpefunktion til eval. Her er al logikken. *)
  fun programToDrawing((x, y), [], _) = [(x, y)]
      | programToDrawing((x, y), c::cs, h) =
          case c of
            Turn => programToDrawing((x, y), cs, turnRight(h))
            | Move c => (x, y)::programToDrawing( move(h, (x, y), Move c),
                                                  cs, h)
            | _ => programToDrawing((x, y), cs, h)
in
  (* Omdanner et program til en Drawing, så det kan tegnes i SVG *)
  fun eval((p, h), cl) = programToDrawing( p,
                                           optimise(removeRepeats(cl)),
                                           h )
end

(* Laver en bedre optimering af skilpaddeprogrammet, ved at lære den at gå
   baglæns når den kan. *)
fun singleTurnsOnly[] = []
  | singleTurnsOnly(x::xs) =
    let
      fun isMove(Move n) = true
        | isMove(_) = false

      (* Bruges til at flippe fortegnet på move. *)
      fun negate(Move n) = Move (~n)
        | negate(x) = x

      (* Fjerner alle Turns, hvor der er kun 2 turns i træk foran et Move *)
      fun removeEvens([]) = []
        | removeEvens([x]) = [x]
        | removeEvens(x::y::xss) =
          if x = Turn andalso y = Turn then
            removeEvens(xss) else x::removeEvens(y::xss)

      (* Bytte fortegn på alle de moves der står før en Turn.
         Den skal kaldes efter removeEvens. *)
      fun swap([]) = []
        | swap([x]) = [x]
        | swap(x::y::xss) =
          if x = Turn andalso isMove(y) then
            x::(negate(y))::swap(xss) else x::swap(y::xss)
    in
      swap(removeEvens(optimise(x::xs)))
    end

(* -------------------------------TESTS------------------------------------- *)

(* Test af funktionen firstDefined *)
val testFirstDefined = firstDefined([NONE, SOME 7, SOME 3, NONE]) = SOME 7;
val testFirstDefined = firstDefined([NONE, NONE]) = NONE;
val testFirstDefined = firstDefined([NONE]) = NONE;
val testFirstDefined = firstDefined([SOME 1]) = SOME 1;

(* Test af funktionen allDefined *)
val testAllDefined = allDefined([NONE, SOME 7, SOME 3, NONE, SOME 7]) = [7, 3, 7];
val testAllDefined = allDefined([NONE, NONE]) = [];

(* Test af funktionen subsequence *)
val testSubsequence = subsequence("abba", "abracadabra");
val testSubsequence = subsequence("aab", "abba") = false;
val testSubsequence = subsequence("aba", "aba") = true;

(* Test af funktionen digitDivisible *)
val testDigitDivisible = digitDivisible(315);
val testDigitDivisible = not(digitDivisible(513));
val testDigitDivisible = not(digitDivisible(0));
val testDigitDivisible = not(digitDivisible(10));

(* Test af funktionen digitDivisibleBetween*)
val testDigitDivBetween = digitDivisibleBetween(10, 30) = [11, 12, 15, 22, 24];
val testDigitDivBetween = digitDivisibleBetween(25, 30) = [];
val testDigitDivBetween = digitDivisibleBetween(30, 10) = [];

(* Test af funktionen countDown *)
val testCountDown  = countDown(3) = [3, 2, 1, 0];
val testCountDown  = countDown(0) = [0];

(* Her fanger jeg en untagelse og sørger for at der bliver retuneret
   en tom liste, som der testes på. Undladelse ville fremføre
   en terminering af programmet.*)

val testCountDownDomain  = (countDown(~1) handle Domain => []) = [];

(* Test af funktionen orderedList *)
val testOrderedList = orderedList([]);
val testOrderedList = not(orderedList([1,2,3,4]));
val testOrderedList = orderedList( rev [1,2,3,4]);
val testOrderedList = not(orderedList( [4,3,2,4]));
val testOrderedList = orderedList( [4,3,2,2]);

(* Test af funktionen nns *)
val testnns = nns 0 = [];
val testnns = nns 1 = [1];
val testnns = nns 4 = [1, 2, 2, 3, 3, 3, 4, 4, 4, 4];

(* Igen håndterer jeg undtagelsen ved at returnere en "ikke-defineret"
   værdi, og tester om den kaster undtagelsen *)
val testnnsDomain = (nns ~1 handle Domain => [~1]) = [~1];

(* Test drawingToSVG *)
local
  val drawing1 = [(0, 0), (100, 100), (200, 0), (400, 78)];
in
  val tegningTest = drawingToSVG "tegning.svg" drawing1
end

(* Tests opgave 6 *)
local
  val aProgram  = [Move 5, Move 37];
  val aProgram1 = [Move 5, Turn, Turn, Turn, Turn, Move 37];
  val aProgram2 = [Move 5, Turn, Turn, Repeat(0, [Move 5]),
                   Turn, Turn, Move 37];
  val aProgram3 = [Move 100, Turn, Turn, Turn, Move 200, Turn, Move 100,
                                                  Turn,  Turn, Move 500];

in
  val testSingle = singleTurnsOnly( aProgram3 ) = [Move 100, Turn, Move ~200,
                                                   Turn, Move ~100, Move 500];
  val testOptimise = optimise(aProgram2) = [Move 42];
  val testRmvRepeats = removeRepeats([Repeat(0, [Move 5])]) = [];
  val testRmvRepeats = removeRepeats([Repeat(1, [Move 5])]) = [Move 5];
  val testEval = eval(((1,1), North), [Move 37, Move 5]) = [(1, 1), (1, ~41)];
end
