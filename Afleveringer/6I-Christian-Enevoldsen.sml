(* 6I-Christian Enevoldsen *)

(* Ass. 6I1, 15.1, HR *)

datatype Question = QUESTION of (order * int);

open TextIO;

local
  (* Retrieves the user input *)
  fun inLine() = inputLine(stdIn);

  (* Prints the string s followed by a line break *)
  fun print(s) = ( output(stdOut, s ^"\n") ; flushOut(stdOut) );

  (* Prints out the syntax needed for this program to work *)
  fun errmsg() = print("Ask questions of form: \"< n\", \"= n\" or \"> n\"");

  local

   (* Returns the operator, which will later be converted to an order *)
    fun getOperator(s) = String.sub(s, 0);

    (* Returns the number the user entered *)
    fun getInt(s) = valOf (Int.fromString(substring(s, 2, size(s)-2)));

    (* Converts the raw input to a Question datatype which the program uses for its logic *)
    fun createQuestion(s) =
      let val or = getOperator(s) val n = getInt(s)
      in
        if or = #"<" then QUESTION(LESS, n) else
        if or = #"=" then QUESTION(EQUAL, n) else
        QUESTION(GREATER, n)
      end;

    (* Computes a Response tuple, that contains information regarding whether the user
     * has one clue right and or if it has found the number. *)
    fun compareQuestion(QUESTION(order, n), q) = case order of
      LESS => (q < n, false )
    | EQUAL => (n = q, true )
    | GREATER => (q > n, false)

    (* Uses the information from compareQuestion to determine an answer to the user *)
    fun respond(a, q) = case compareQuestion(a, q) of
        (false, _) => print("NO")
      | (true, false) => print("YES")
      | (true, true) => print("You found it");

    (* Checks if an element is in a list *)
    fun contains(_, []) = false
      | contains(a, [x]) = x = a
      | contains(a, (x::xs)) = x = a orelse contains(a, xs);

    (* Validates user input. Returns true if verifaction is succesful, false otherwise *)
    (* The logic could be written better, but for now this will do. *)
    fun validate(s) =
      let
        val chars = explode(String.substring(s, 2, size(s)-2))
        val part = List.partition Char.isDigit chars
        val tup = (String.sub(s, 0), String.sub(s, 1), Int.fromString(substring(s, 2, size(s) - 2)))
        fun validPart (x, []) = true
          | validPart([], x) = false
          | validPart(x, [y]) = y = #"\n"
          | validPart (_, _) = false
        fun isOrder a = contains(a, [#">", #"<", #"="])
        fun f((x, y, z)) = isOrder(x)
            andalso Char.isSpace(y)
            andalso z <> NONE
            andalso validPart(part)
      in
        f(tup)
      end;

  in

    (* This is the main game loop, which prompts input from the user until
     * the user has guessed the number, which will terminate the quiz program.
     *)
    fun questionLoop(theNum) =
      let
        fun isFound(a, q) = compareQuestion(a, q) = (true, true)
        val q = valOf(inLine())
      in
        case (validate(q)) of
            true => (
                let val data_q = createQuestion(q) in
                respond(data_q, theNum) ;
                  if not(isFound(data_q, theNum))
                  then questionLoop(theNum)
                  else print("Game Over")
                end
              )
          | false => (errmsg() ; questionLoop(theNum))
      end;
    end

  (* Random information needed for the user to understand the game *)
  fun prelude(min, max) =
    ( output(stdOut, "You should guess a number between ")
    ; output(stdOut, Int.toString(min))
    ; output(stdOut, " and ")
    ; output(stdOut, Int.toString(max))
    ; output(stdOut, "\n")
    ; flushOut(stdOut)
  );

  val rg = Random.newgen();
in
  (* The quiz program. Call this method to start the game! *)
  fun quiz() = (prelude(0, 59) ; errmsg() ; A())
  and A() = questionLoop(Random.range(0, 59) rg);

end;

quiz()
