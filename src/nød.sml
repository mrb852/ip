fun strOfStars 0 = ""
|	strOfStars(n) = "*"^strOfStars(n-1);

fun strOfSpaces 0 = ""
|	strOfSpaces(n) = " " ^ strOfSpaces(n-1);

fun strInsert(str1, str2, i) = substring(str1, 0, i) ^ str2 ^ substring(str1, i, size(str1) - i);

fun spacesInLayer(l, n) = (n-l) * 2;
fun starsInLayer(0) = 0
|	starsInLayer(1) = 1
|	starsInLayer(n) = starsInLayer(n-1) + 2;

fun treeLayer(l, n) = strInsert( 
		strOfSpaces( spacesInLayer(l, n) ), 
		strOfStars( starsInLayer(l) ), 
		spacesInLayer(l, n) div 2
	) ^ "\n";

fun buildTree(l, m) = 
	if m<l then "" else treeLayer(l, m) ^ buildTree(l+1, m);

fun tree(n) = buildTree(1, n);