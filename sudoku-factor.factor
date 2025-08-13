! Copyright (C) 2025 Your name.
! See https://factorcode.org/license.txt for BSD license.

USING: tools.time math assocs io io.files json sequences kernel prettyprint formatting ;

IN: sudoku-factor

! A helper word to print a single cell.
! If the cell is 0, it prints a '.' for an empty space.
! Otherwise, it prints the number.
: sudoku-cell-pp ( n -- )
    dup 0 =
    [ drop "." printf ]
    [ "%d" printf ]
    if ;

! Prints a single row of the Sudoku board with vertical separators.
: sudoku-row-pp ( row -- )
    [| cell index |
        " " printf
        cell sudoku-cell-pp
        ! After the 3rd and 6th cells, print a separator.
        index 1 + { 3 6 } member? [ " |" printf ] when
    ] each-index
    nl ;

! Takes a 9x9 board (array of arrays) from the stack and pretty-prints it.
: sudoku-board-pp ( board -- )
    " -----------------------" print
    [| row index |
        row sudoku-row-pp
        ! After the 3rd and 6th rows, print a horizontal separator.
        index 1 + { 3 6 } member? [ "-------+-------+-------" print ] when
    ] each-index
    " -----------------------" print ;

: extract-board ( obj -- grid )
    "GRID" swap at
;

: print-name ( obj -- )
    "NAME" swap at "Problem %s\n" printf
;

! Finds the first empty cell (value 0) in the board.
! Returns row col, or f f if no empty cells are found.
: find-empty ( board -- row/f col/f )
    0 swap [ 0 swap index-from ] find-from ;

! Checks if placing a number 'n' at a given (row, col) is valid.
: is-valid? ( board n row col -- ? )
    [| board n row col |
        ! Check row
        board row nth n member? not
        [
            ! Check column
            board [ col nth ] map n member? not
            [
                ! Check 3x3 subgrid, using 'dip'
                row 3 /i 3 * ! ( box_row )
                col 3 /i 3 * ! ( box_row box_col )

                ! Dip under box_col to slice the rows from the board
                [ board swap 3 + >rect ] dip ! ( box_col subgrid-rows )

                ! Curry box_col into a slice op, and map over the rows
                [ 3 + >rect ] swapd curry map ! ( subgrid-2d )

                flatten                     ! ( subgrid-1d )
                n member? not               ! ( ? )
            ] &&
        ] &&
    ] with-scope ;

! Sets a value in a 2D array (the board).
: set-cell-2d ( value board row col -- new-board )
    [ set-nth-fast ] 2curry change-nth ;

! Solves the board using a backtracking algorithm.
! Returns a solved board, or f if no solution is found.
: solve ( board -- solved-board/f )
    dup find-empty [| row col |
        ! Base case: No empty cells found, the board is solved.
        2drop
    ] [| board row col |
        ! Recursive step: Try numbers 1-9 in the empty cell.
        9 [1,b] [| n |
            board n row col is-valid? [
                ! If valid, place the number and recurse.
                board deep-clone n swap row col set-cell-2d solve
            ] [ f ] if
        ] find nip ! find returns the first non-f result from the attempts.
    ] if ;



: process-problem ( obj -- )  
    dup print-name
    extract-board
    dup sudoku-board-pp
    [ solve ] benchmark
    "\nSolution:\n" printf
    swap sudoku-board-pp
    time.
    "\n\n" printf
;  inline

: process-problem-file ( path  -- )
  path>json 10 head [ process-problem ] each
; 

: solve-all ( -- )  "/home/yoel/Development/Factor/factor/work/sudoku-factor/problems.json"  process-problem-file ;
