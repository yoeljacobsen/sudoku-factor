! Copyright (C) 2025 Your name.
! See https://factorcode.org/license.txt for BSD license.

USING: tools.time math assocs io io.files json sequences kernel prettyprint formatting
       columns combinators combinators.short-circuit generalizations
       io.styles io.pathnames math.parser namespaces unicode
;

IN: sudoku-factor

SYMBOL: solutions
SYMBOL: board

: pair+ ( a b c d -- a+b c+d ) swapd [ + ] 2bi@ ;

: row ( n -- row ) board get nth ;
: board> ( m n -- x ) row nth ;
: >board ( row m n -- ) row set-nth ;

: row-any? ( n y -- ? ) row member? ;
: col-any? ( n x -- ? ) board get swap <column> member? ;
: cell-any? ( n x y i -- ? ) 3 /mod pair+ board> = ;

: box-any? ( n x y -- ? )
    [ 3 /i 3 * ] bi@ 9 <iota> [ cell-any? ] 3 nwith any? ;

: board-any? ( n x y -- ? )
    { [ nip row-any? ] [ drop col-any? ] [ box-any? ] } 3|| ;

DEFER: search

: assume ( n x y -- )
    [ >board ] [ [ 1 + ] dip search f ] [ >board ] 2tri ;

: attempt ( n x y -- )
    3dup board-any? [ 3drop ] [ assume ] if ;

: solve ( x y -- )
    9 [ 1 + 2over attempt ] each-integer 2drop ;

: cell. ( cell -- )
    [ [ number>string write ] [ "." write ] if* ] with-cell ;

: row. ( row -- )
    [ [ cell. ] each ] with-row ;

: board. ( board -- )
    standard-table-style [ [ row. ] each ] tabular-output nl ;

: solution. ( -- )
    solutions inc "Solution:" print board get board. ;

: search ( x y -- )
    {
        { [ over 9 = ] [ [ drop 0 ] dip 1 + search ] }
        { [ over 0 = over 9 = and ] [ 2drop solution. ] }
        { [ 2dup board> ] [ [ 1 + ] dip search ] }
        [ solve ]
    } cond ;

: sudoku ( board -- )
    [
        "Puzzle:" print dup board.

        0 solutions set
        [ clone ] map board set

        0 0 search

        solutions get number>string write " solutions." print
    ] with-scope ;

: replace-zeros ( 2d-array -- 2d-array )
    [ [ dup 0 = [ drop f ] when ] map! ] map ;

: extract-board ( obj -- grid )
    "GRID" swap at replace-zeros
;

: print-name ( obj -- )
    "NAME" swap at "Problem %s\n" printf
;

: process-problem ( obj -- )
    dup print-name
    extract-board
    [ sudoku ] benchmark
    time.
    "\n\n" printf
;  inline

: process-problem-file ( path  -- )
  path>json 100 head [ process-problem ] each
; 

: solve-all ( -- )  current-directory get "work/sudoku-factor/problems.json" string-append  process-problem-file ;
