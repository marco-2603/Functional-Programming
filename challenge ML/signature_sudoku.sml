signature SUDOKU =
sig
	type cell
	type board
  	

	val readSudoku : string -> board
	val printBoard : board -> unit
	val verifySudoku : board -> bool


end;

structure Sudoku : SUDOKU =
struct
	type cell = (int*int*int)
	type board = cell list
	
	(* read Sudoku *)
	fun loopChars ins row col =
    		case TextIO.input1 ins of
        		NONE => []
      			| SOME ch =>
          			if Char.isDigit ch then
            				let
              					val value = valOf (Int.fromString (String.str ch))
              					val newCell = (value, row, col)
            				in
              					if col = 9 then
                					newCell:: (loopChars ins (row + 1) 1 )
              					else
                					newCell:: (loopChars ins row (col + 1) )
            				end
          			else
            				loopChars ins row col 

  	fun readSudoku filename =
    		let
      			val inputFile = TextIO.openIn filename
      			val digits = loopChars inputFile 1 1 (* Starting with indexes row=1, col=1 *)
      			val _ = TextIO.closeIn inputFile
    		in
      			digits
    		end
    
	(* print Sudoku *)
	fun filter f nil = nil
	|filter f (x::xs) = if f(x) then x::(filter f xs) else (filter f xs);
	
	fun retrieve_cell row_idx col_idx sudoku = hd (filter(fn(x,r,c) => r=row_idx andalso c=col_idx) sudoku)

	fun printCell (x,r,c) = (print(Int.toString(x));print(" "));
	fun printRow row_idx 10 sudoku = print ("\n")
		|printRow row_idx col_idx sudoku = (printCell(retrieve_cell row_idx col_idx sudoku); printRow row_idx (col_idx+1) sudoku);
		
	fun printGrid 10 sudoku = ()
		|printGrid row_idx sudoku = (printRow row_idx 1 sudoku; printGrid (row_idx+1) sudoku);

  	fun printBoard sudoku = printGrid 1 sudoku;
	
	
	(** to be filled **)
	
	fun verifySudoku sudoku = true;

end:>SUDOKU;