/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/* this file contains the magnets puzzle solver */

:-consult(utils).

/* gets the puzzle element on the specified row and column */
getPuzzleElement(Element, Puzzle, PuzzleSize, Row, Column) :-
        Position is (Row - 1) * PuzzleSize + Column , 
        nth1(Position, Puzzle, Element).

/* gets the row and column corresponding to a position */
getRowColumn(Position, Size, Row, Column) :-
        Row is div(Position, Size) + 1 ,
        Column is mod(Position, Size).

/* obtains the position and cardinal of the other 
   half of a domino, given the position and cardinal of one half */
getOtherHalf(Row1, Column1, n, Row2, Column2, s) :- 
        Row2 is Row1 - 1 , Column2 is Column1. 
getOtherHalf(Row1, Column1, e, Row2, Column2, w) :- 
        Row2 is Row1 , Column2 is Column1 + 1. 
getOtherHalf(Row1, Column1, s, Row2, Column2, n) :- 
        Row2 is Row1 + 1 , Column2 is Column1. 
getOtherHalf(Row1, Column1, w, Row2, Column2, e) :- 
        Row2 is Row1 , Column2 is Column1 - 1.

/* checks the polarity constraint on a magnet: sum of poles must be 0 */
checkMagnet(Puzzle, Result, PuzzleSize, Row, Column) :-
        getPuzzleElement(Cardinal, Puzzle, PuzzleSize, Row, Column) ,
        (member(Cardinal, [w, n]) -> true ;
         (getOtherHalf(Row, Column, Cardinal, Row2, Column2, _) ,
          getPuzzleElement(Pole1, Result, PuzzleSize, Row, Column) ,
          getPuzzleElement(Pole2, Result, PuzzleSize, Row2, Column2) ,
          Pole1 + Pole2 #= 0)).

/* checks the polarity constraint on all magnets 
   starting in the given row and column */
checkAllMagnets(Puzzle, Result, PuzzleSize, Row, Column) :-
        Row > PuzzleSize -> true ; 
        (Column > PuzzleSize -> 
         (Row1 is Row + 1 , 
          checkAllMagnets(Puzzle, Result, PuzzleSize, Row1, 1)) ;
         (checkMagnet(Puzzle, Result, PuzzleSize, Row, Column) ,
         Column1 is Column + 1 ,
         checkAllMagnets(Puzzle, Result, PuzzleSize, Row, Column1))).
         
/* checks the polarity constraint on all magnets */
checkAllMagnets(Puzzle, Result, PuzzleSize) :-
        checkAllMagnets(Puzzle, Result, PuzzleSize, 1, 1).

/* gets the number of zeros in a row or column */
getRCzeros([], _, [], []).
getRCzeros([RCzero|RCzeros], PuzzleSize, [RCminu|RCminus], [RCplu|RCplus]) :-
        RCzero is PuzzleSize - (RCminu + RCplu) ,
        getRCzeros(RCzeros, PuzzleSize, RCminus, RCplus).

/* computes the frequency lists for the rows and columns to use with global_cardinality */
getFrequencies([], [], [], []).
getFrequencies([[-1-Mfreq, 0-Zfreq, 1-Pfreq]|RCfrequencies], [RCminu|RCminus], [RCzero|RCzeros], [RCplu|RCplus]) :-
        Mfreq is RCminu , Zfreq is RCzero , Pfreq is RCplu ,
        getFrequencies(RCfrequencies, RCminus, RCzeros, RCplus).        

/* checks the global_cardinality constraint for a list of rows or columns */
checkFrequency([], []).
checkFrequency([RC|RCs], [RCfrequency|RCfrequencies]) :-
        global_cardinality(RC, RCfrequency) ,
        checkFrequency(RCs, RCfrequencies).

/* transforms a flat puzzle list into a matrix of rows */
unflatten([], [], _, _).
unflatten([Element|Puzzle], [[Element|Row]|RowsPuzzle], PuzzleSize, Counter) :-
        Counter >= PuzzleSize -> 
        (Row = [] ,
         unflatten(Puzzle, RowsPuzzle, PuzzleSize, 1)) ;
        (Counter1 is Counter + 1 ,
         unflatten(Puzzle, [Row|RowsPuzzle], PuzzleSize, Counter1)).
unflatten(Puzzle, RowsPuzzle, PuzzleSize) :- 
        unflatten(Puzzle, RowsPuzzle, PuzzleSize, 1).
        
/* checks the global_cardinality constraint for the whole puzzle */
checkAllFrequencies(Result, PuzzleSize, Rminus, Rplus, Cminus, Cplus) :-
        unflatten(Result, ResultRows, PuzzleSize) ,
        transpose(ResultRows, ResultColumns) ,
        getRCzeros(Rzeros, PuzzleSize, Rminus, Rplus) ,
        getRCzeros(Czeros, PuzzleSize, Cminus, Cplus) ,
        getFrequencies(Rfrequencies, Rminus, Rzeros, Rplus) ,
        getFrequencies(Cfrequencies, Cminus, Czeros, Cplus) ,
        checkFrequency(ResultRows, Rfrequencies) ,
        checkFrequency(ResultColumns, Cfrequencies).        

/* computes the eastern and shouthern neighbors of a given pole in the puzzle */
getEastSouth(Result, PuzzleSize, Row, Column, East, South) :-
        Row == PuzzleSize ->
        (South = 0 , 
         (Column == PuzzleSize ->
          East = 0 ;
          (Column1 is Column + 1 ,
           getPuzzleElement(East, Result, PuzzleSize, Row, Column1)))) ;
        (Column == PuzzleSize ->
         (East = 0 ,
          Row1 is Row + 1 ,
          getPuzzleElement(South, Result, PuzzleSize, Row1, Column)) ;
         (Row1 is Row + 1 ,
          Column1 is Column + 1 ,
          getPuzzleElement(East, Result, PuzzleSize, Row, Column1) ,
          getPuzzleElement(South, Result, PuzzleSize, Row1, Column))).

/* checks the repelling condition (poles with the 
   same charge cannot be vertically or horizontally 
   adjacent) for a given pole in the puzzle */
checkRepelling(Result, PuzzleSize, Row, Column) :-
        getPuzzleElement(Pole, Result, PuzzleSize, Row, Column) ,
        getEastSouth(Result, PuzzleSize, Row, Column, East, South) ,
        Pole + East #< 2 ,
        Pole + East #> -2 ,
        Pole + South #< 2 ,
        Pole + South #> -2 .

/* checks the repelling condition starting 
   in the given row and column */
checkAllRepelling(Result, PuzzleSize, Row, Column) :-
        Row > PuzzleSize -> true ;
        (Column > PuzzleSize ->
         (Row1 is Row + 1 ,
          checkAllRepelling(Result, PuzzleSize, Row1, 1)) ;
         (checkRepelling(Result, PuzzleSize, Row, Column) ,
          Column1 is Column + 1 ,
          checkAllRepelling(Result, PuzzleSize, Row, Column1))).

/* checks the repelling condition for the whole puzzle */
checkAllRepelling(Result, PuzzleSize) :-
        checkAllRepelling(Result, PuzzleSize, 1, 1).

/* uses the constraints above to solve the puzzle */
solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, Options) :-
        Square is PuzzleSize * PuzzleSize ,
        length(Result, Square) ,
        checkAllFrequencies(Result, PuzzleSize, Rminus, Rplus, Cminus, Cplus) ,
        checkAllMagnets(Puzzle, Result, PuzzleSize) ,
        checkAllRepelling(Result, PuzzleSize) ,
        labeling(Options, Result).


/************/
/* examples */
/************/

e1Puzzle([e, w, e, w, e, w, 
          s, e, w, e, w, s, 
          n, e, w, e, w, n, 
          s, s, s, s, e, w, 
          n, n, n, n, s, s, 
          e, w, e, w, n, n]).
e1PuzzleSize(6).
e1Rminus([1, 2, 2, 2, 1, 2]).
e1Rplus([1, 1, 3, 2, 2, 1]).
e1Cminus([2, 1, 1, 2, 1, 3]).
e1Cplus([2, 2, 0, 2, 2, 2]).
e1Result([-1, 1, 0, 0, 0, 0, 
           0, 0, 0,-1, 1,-1,
           0, 1,-1, 1,-1, 1,
           1, 0, 0,-1, 1,-1,
          -1, 0, 0, 1, 0, 1,
           1,-1, 0, 0, 0,-1]).

e2Puzzle([e, w, s, e, w, e, w, s, s, s,
          s, s, n, e, w, e, w, n, n, n,
          n, n, s, e, w, s, s, s, e, w,
          s, s, n, s, s, n, n, n, e, w,
          n, n, s, n, n, e, w, s, s, s,
          s, s, n, s, s, s, s, n, n, n,
          n, n, s, n, n, n, n, e, w, s,
          s, s, n, e, w, e, w, e, w, n,
          n, n, s, s, e, w, e, w, s, s,
          e, w, n, n, e, w, e, w, n, n]).
e2PuzzleSize(10).
e2Rminus([4, 2, 3, 4, 2, 3, 3, 2, 3, 3]).
e2Rplus([3, 3, 2, 3, 2, 4, 3, 3, 2, 4]).
e2Cminus([3, 2, 3, 4, 2, 3, 3, 3, 3, 3]).
e2Cplus([2, 3, 3, 3, 2, 2, 4, 3, 3, 4]).
e2Result([ 0, 0, 0,-1, 1,-1, 1,-1, 1,-1,
           1,-1, 0, 0, 0, 0, 0, 1,-1, 1,
          -1, 1,-1, 0, 0, 0, 1,-1, 0, 0,
           0,-1, 1,-1, 0, 0,-1, 1,-1, 1,
           0, 1,-1, 1, 0, 0, 0, 0, 0,-1,
          -1, 0, 1,-1, 1,-1, 1, 0, 0, 1,
           1, 0,-1, 1,-1, 1,-1, 0, 0, 0,
           0, 0, 1, 0, 0,-1, 1,-1, 1, 0,
           0, 0, 0,-1, 0, 0,-1, 1,-1, 1,
          -1, 1, 0, 1,-1, 1, 0, 0, 1,-1]).
           
solveE1(E1Result) :-
        e1Puzzle(Puzzle) , 
        e1PuzzleSize(PuzzleSize) ,
        e1Rminus(Rminus) ,
        e1Rplus(Rplus) ,
        e1Cminus(Cminus) ,
        e1Cplus(Cplus) ,
        solvePuzzle(E1Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, [ff, median]).

solveE2(E2Result) :-
        e2Puzzle(Puzzle) , 
        e2PuzzleSize(PuzzleSize) ,
        e2Rminus(Rminus) ,
        e2Rplus(Rplus) ,
        e2Cminus(Cminus) ,
        e2Cplus(Cplus) ,
        solvePuzzle(E2Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, [ff, median]).        


