(* Gruppeaflevering uge 3, lavet af
 * Benjamin Rotendahl, Nicklas Warming Jacobsen,
 * Jacob Harder og Christian Hohlmann Enevoldsen
 *)

fun readAll filename =
    let
      val fd = TextIO.openIn filename
    in
      TextIO.inputAll fd
      before TextIO.closeIn fd
    end

(* 3G1 *)

fun splitAt (c, s) =
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

val test_splitAt_1 = splitAt (#"0", "0123401234012340")

(* 3G2 *)

fun firstChar "" = raise Fail "No first char"
  | firstChar s = hd (explode s)

val test_firstChar_1 = #"a" = firstChar "asd"

fun findWordsLine [] = raise Fail "No lexicon found"
  | findWordsLine (s :: ss) = if firstChar s = #"%"
                              then implode (tl (explode s))
                              else findWordsLine ss

val test_findWordsLine_1 =
    "asd" = findWordsLine ["e","%asd","123","opiw"]

(* 3G3 *)

 fun splitWordsLine s =
     let
       val leksikon = []
       fun makeList s = splitAt(#",",s) @ leksikon
     in
       makeList s
     end


(* 3G4 *)

fun findWord (ws : string list, n : int) =
    if n<1
    then raise Fail "Bad index (lexicon)"
    else List.nth (ws, n-1)

val test_findWord_1 = "123" = findWord (["asd","123","qwe","zxc"], 2)
val test_findWord_2 = "qwe" = findWord (["asd","123","qwe","zxc"], 3)


(* Opgave 3G5 *)
(* removeNonTracks : string list -> string list *)

fun removeNonTracks list =
    let
      fun helper (c, []) = c
        | helper (c, x::xs) =
                helper ((if size x = 0 orelse String.substring(x, 0, 1) = "#"
                    orelse String.substring(x, 0, 1) = "%"
                 then c else x::c), xs)
    in
      helper([], list)
    end

(* removeNonTracks test *)
(*
val removeNonTracksTestSet = ["# TESTING SET",
"# MusiXmatch dataset, the official lyrics dataset",
"# of the Million Song Dataset",
"#    file created on Tue Mar 29 04:28:44 2011",
"#    contact: T. Bertin-Mahieux (Columbia University)",
"#             tb2332@columbia.edu",
"#    also: http://labrosa.ee.columbia.edu/millionsong/musixmatch",
"#          http://www.musixmatch.com",
"%i,the,you,to,and,a,me,it,not,in,my,is,of,your,that,do,on,are,we,am,will,all,for",
"TRAADQW128F427CE68,3811449,1:3,2:3,3:2,7:4,8:1,9:3,11:1,12:1,13:1,14:2,20:1",
"TRAADRX12903D0EFE8,5583484,1:1,6:5,7:1,10:1",
"",
"TRAAEJQ128F92C484E,9124657,1:28,2:7,3:12,4:3,5:4,6:3,7:1,8:11,9:3,10:1,11:3,12:8"];

val removeTrackTest1 = removeNonTracks(removeNonTracksTestSet) =
    [
      "TRAAEJQ128F92C484E,9124657,1:28,2:7,3:12,4:3,5:4,6:3,7:1,8:11,9:3,10:1,11:3,12:8",
      "TRAADRX12903D0EFE8,5583484,1:1,6:5,7:1,10:1",
      "TRAADQW128F427CE68,3811449,1:3,2:3,3:2,7:4,8:1,9:3,11:1,12:1,13:1,14:2,20:1"
    ]
*)

(* Opgave 3G6 *)
(* splitTrack string -> string * string * (int * int) list *)

fun splitTrack line =
    let
      fun stringToInt (i, "") = i
        | stringToInt (i, s)  =
          let
            fun translator "1" = 1
              | translator "2" = 2
              | translator "3" = 3
              | translator "4" = 4
              | translator "5" = 5
              | translator "6" = 6
              | translator "7" = 7
              | translator "8" = 8
              | translator "9" = 9
              | translator "0" = 0
              | translator  s  = raise Fail "Cannot translate 9<"
          in
            stringToInt(i*10 + translator(String.substring(s, 0, 1)),
                        String.substring(s, 1, size s - 1))
          end

      fun indexAt (i, s, x) =
          if i < size s then
            if String.substring(s, i, 1) = x then i else indexAt(i+1, s, x)
          else
            i;
      fun createLeksikon (leks, "") = leks
        | createLeksikon (leks, s) =
          let
            val IDxi = indexAt(0, s, ":")
            val CNtxi = indexAt(IDxi+1, s, ",")
            val IDx = String.substring(s, 0, IDxi)
            val CNtx = String.substring(s, IDxi+1, CNtxi-IDxi-1)
            val rest = (if CNtxi < size s then
                          String.substring(s, CNtxi+1, size s - (CNtxi+1))
                        else
                          ""
                       )
          in
            createLeksikon((stringToInt(0, IDx), stringToInt(0, CNtx)) :: leks, rest)
          end

      val TIDi = indexAt (0, line ,",")
      val MXMIDi = indexAt(TIDi+1, line, ",")
      val LEKSISi = MXMIDi+1;
    in
      (
        String.substring(line, 0, TIDi),
        String.substring(line, TIDi+1, MXMIDi-TIDi-1),
        createLeksikon([], String.substring(line, LEKSISi, size line - LEKSISi))
      )
    end

(* splitTrack test *)
(*
val testSplitTrack = splitTrack(
      "TRAAEJQ128F92C484E,9124657,1:28,2:7,3:12,4:3,5:4,6:3,7:1,8:11,9:3,10:1,11:3,12:8"
) = (
      "TRAAEJQ128F92C484E",
      "9124657",
      [
        (12, 8), (11, 3), (10, 1), (9, 3), (8, 11), (7, 1), (6, 3), (5, 4),
        (4, 3), (3, 12), (2, 7), (1, 28)
      ]
    )
*)

(* 3G7 *)

fun readMXM filename =
    let
      val data = readAll filename
      val dataLines = splitAt (#"\n", data)
      val lexiconLine = findWordsLine dataLines
      val lexicon = splitWordsLine lexiconLine
      val trackLines = removeNonTracks dataLines
      val tracks = map splitTrack trackLines
      fun index2Word lexicon (id, n) =
          (findWord (lexicon, id), n)
      fun indexes2Human lexicon indexTuples =
          map (index2Word lexicon) indexTuples
      fun track2Human lexicon (TID, MXMID, indexTuples) =
          (TID, indexes2Human lexicon indexTuples)
    in
      map (track2Human lexicon) tracks
    end

val test_readMXM_1 = readMXM "mxm_mini.txt"
(*val test_readMXM_2 = readMXM "mxm_dataset_1000.txt" *)

(*
(* 3G8 *)

fun mostWords tracksInfo =
    let
      fun countWords m (TID, []) = (TID, m)
        | countWords m (TID, (_, n) :: tuples) =
          countWords (m + n) (TID, tuples)
      fun countList tInfo = map (countWords 0) tInfo
    in
      countList tracksInfo
    end
*)
