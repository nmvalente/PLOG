
/* 
the lists of lists (rows of columns) are flattened,
for example, [[a, b], [c, d]] is represented as [a, b, c, d].
*/

/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- consult(display).
:- consult(stats).

/* asks the user for the size of the puzzle */
getPuzzleSize(Number) :- 
        prompt(_, 'Choose an even puzzle size: ') ,
        getDoubleDigit(Numbert) , 
        (Numbert < 2 -> 
         (write('Must be at least 2.') , nl , getPuzzleSize(Number)) ; 
         (even(Numbert) ->
          Number is Numbert ;
          (write('Must be an even number.') , nl , getPuzzleSize(Number)))).

/* predicate used to check if the player wishes to see the solution */
getSolution :- 
        prompt(_, 'Press enter to get the solution. ') ,   /* give information */
        getChar(_).                                        /* wait for answer */

/* predicate used to check if the player wishes to finish */
getFinish :- 
        prompt(_, 'Press enter to finish. ') ,             /* give information */
        getChar(_).                                        /* wait for answer */
       
playAgain :- 
        prompt(_, 'Play again (y/n)? ') , 
        getChar(C) ,
        (C == 'y' -> consult(magnets) ; halt).

playPuzzle :-
        cls , nl , 
        write('Welcome to Magnets!!!') , nl , nl , 
        getPuzzleSize(PuzzleSize) ,
        getRandomPuzzle(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus) ,
        unflatten(Puzzle, RowsPuzzle, PuzzleSize) ,                                  
        cls , nl , 
        printPuzzle(RowsPuzzle, Rplus, Rminus, Cplus, Cminus) ,
        nl , nl ,
        getSolution ,
        solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, [median]) ,
        unflatten(Result, RowsResult, PuzzleSize) ,
        cls , nl , 
        printResult(RowsPuzzle, RowsResult, Rplus, Rminus, Cplus, Cminus) ,
        nl , nl ,
        getFinish ,
        nl , nl ,
        playAgain.

/* always begin by calling startGame */
:- initialization(playPuzzle).

     

   







/* 
the initial puzzle is represented by a list Puzzle of rows Row.
each row Row is a list of columns Column. each column Column contains 
the Cardinal (n, e, s, w) of the other half of the magnet in that position.
finally there are four lists that complete the initial puzzle:
. Rminus - list with the number of negative poles in each row;
. Rplus - list with the number of positive poles in each row;
. Cminus - list with the number of negative poles in each column;
. Cplus - list with the number of negative poles in each column.
*/

/* 
the resolved puzzle is represented by a list Result of rows Row.
each row Row is a list of columns Column. each column Column contains 
the value of the pole in that position:
. -1 - negative pole;
. 1 - positive pole;
. 0 - neutral pole.
*/

/* the domain of each pole in the result is -1, 0, 1. */

/* 
the restrictions on the poles are:
. the sum of both poles in a magnet must be 0;
. the number of negative poles in each row must match that of Rminus;
. the number of positive poles in each row must match that of Rplus;
. the number of negative poles in each column must match that of Cminus;
. the number of positive poles in each column must match that of Cplu;
. poles with the same polarity cannot touch each other vertically or horizontally.
*/

