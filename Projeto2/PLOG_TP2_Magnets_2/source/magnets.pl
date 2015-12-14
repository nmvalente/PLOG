/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/* this file contains the main application 
   with the user interaction */

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

/* checks if the player wishes to see the solution */
getSolution :- 
        prompt(_, 'Press enter to get the solution. ') ,   
        getChar(_).                                        

/* checks if the player wishes to finish */
getFinish :- 
        prompt(_, 'Press enter to finish. ') ,             
        getChar(_).                                        
       
/* checks if the player wishes to get another puzzle */
playAgain :- 
        prompt(_, 'Play again (y/n)? ') , 
        getChar(C) ,
        (C == 'y' -> consult(magnets) ; halt).

/* prompts the player for input and shows a puzzle and its solution */
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

/* always begin by calling playPuzzle */
:- initialization(playPuzzle).

