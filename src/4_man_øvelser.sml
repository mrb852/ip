(* Mandags opgaver *)

datatype plan = Pl of string | Ma of plan * plan;

(* 4M2 *)
(* Tager to navne og laver en kamp *)
fun makeMacth (name1, name2) = Ma( Pl(name1), Pl(name2) );

(* 4M1 *)
(* Konstruer den tegnede turnering *)
local
  val runde1a = makeMacth("Jens", "Mette");
  val runde1b = makeMacth("Ian", "Helle");
  val runde1c = makeMacth("Yuan", "Anne");
  val runde1d = makeMacth("Yasmin", "Ole");
  val runde1e = makeMacth("Lars", "Ea");
  val runde1f = Pl "Susanne";

  val runde2a = Ma(runde1a, runde1b);
  val runde2b = Ma(runde1c, runde1d);
  val runde2c = Ma(runde1e, runde1f);
in
val turnering = Ma(runde2a, Ma(runde2b,runde2c))
end;

(* 4M3 *)
(* Lav en plan liste ud af navne liste *)

fun makeRound []           = []
  | makeRound [n]          = [Pl n]
  | makeRound (n1::n2::ns) = makeMacth(n1,n2) :: makeRound ns;

(* 4M3½ *)
(* Lav en planliste der er kortere end en givet planliste *)
(* makeNewRound : plan list -> plan list *)
fun makeNewRound []           = []
  | makeNewRound [p]          = [p]
  | makeNewRound (p1::p2::ps) = Ma(p1,p2) :: makeNewRound ps


(* Test *)
val test1 = makeNewRound [Ma(Pl "1", Pl "2"), Pl "3"]
            = [Ma(Ma(Pl "1", Pl "2"), Pl "3")];
val test2 = makeNewRound(makeNewRound (
            makeRound ["Mette", "Jens", "Ian", "Helle", "Susanne"] ))

(* 4M4 *)
(* Laver en tournering ud af en liste af navne *)
(* makeTournament : string list -> plan *)
fun makeTournament names =
  let
    val startList = makeRound names
    fun makeTournament' round =
        case round of
          []      => raise Fail "Invalid playerlist"
        | [p]     => p
        | [m1,m2] => Match(m1,m2) (* bemærk at denne er ligemeget! *)
        | _       => (makeTournament' o makeNewRound) round
  in
    makeTournament' startList
  end;
