fun coinToInt "T" = 2000
|	coinToInt "t" = 1000
|	coinToInt "5" = 500
|	coinToInt "2" = 200
|	coinToInt "1" = 100
| 	coinToInt "h" = 50
| 	coinToInt (c) = 0;

fun countCounsS "" = 0
|	countCounsS(pile) = coinToInt(substring(pile, 0, 1)) + countCounsS(substring(pile, 0, size(pile)-1));

val coins =  countCounsS("tT1h25tt");

"hejsa"
"hejs"