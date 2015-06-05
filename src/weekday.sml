datatype weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun

fun nextDay day = day;

fun nextDay day =
	case day of
			Mon => Tue
		|	Tue => Wed
		|	Wed => Thu
		|	Thu => Fri
		|	Fri => Sat
		|	Sat => Sun
		|	Sun => Mon

fun compareOrder(order, order1) = 
	case order of 
			GREATER
		case order1 of
			LESS
