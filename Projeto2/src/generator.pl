/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- consult(solver).

:- dynamic numberCardinal/1.

getOtherCardinal(Row1, Column1, 1, Row2, Column2, 3) :- 
        Row2 is Row1 - 1 , Column2 is Column1.
getOtherCardinal(Row1, Column1, 2, Row2, Column2, 4) :- 
        Row2 is Row1 , Column2 is Column1 + 1.
getOtherCardinal(Row1, Column1, 3, Row2, Column2, 1) :- 
        Row2 is Row1 + 1 , Column2 is Column1.
getOtherCardinal(Row1, Column1, 4, Row2, Column2, 2) :- 
        Row2 is Row1 , Column2 is Column1 - 1.

checkCardinal(Puzzle, PuzzleSize, Row, Column) :-
        getPuzzleElement(Cardinal, Puzzle, PuzzleSize, Row, Column) ,
        (nonvar(Cardinal) -> true ;
         ((Column == PuzzleSize ->
           Cardinal = 3 ;
           (Row == PuzzleSize ->
            Cardinal = 2 ;
            (Cardinal in 2..3 ,
             numberCardinal(Cardinal),
             retract(numberCardinal(2)) , 
             retract(numberCardinal(3)) ,
             random_permutation([3, 2], [X, Y]) ,
             assert(numberCardinal(X)) , 
             assert(numberCardinal(Y))))))) ,
          getOtherCardinal(Row, Column, Cardinal, Row2, Column2, Cardinal2) ,
          getPuzzleElement(Cardinal2, Puzzle, PuzzleSize, Row2, Column2).

checkAllCardinals(Puzzle, PuzzleSize, Row, Column) :-
        Row > PuzzleSize -> true ; 
        (Column > PuzzleSize -> 
         (Row1 is Row + 1 , 
          checkAllCardinals(Puzzle, PuzzleSize, Row1, 1)) ;
         (checkCardinal(Puzzle, PuzzleSize, Row, Column) ,
          Column1 is Column + 1 ,
          checkAllCardinals(Puzzle, PuzzleSize, Row, Column1))).
         
numbers2Letters([], []).
numbers2Letters([Number|NumberPuzzle], [Letter|LetterPuzzle]) :-
        (Number == 1 -> 
         Letter = n ;
         (Number == 2 ->
          Letter = e ;
          (Number == 3 ->
           Letter = s;
           Letter = w))) ,
        numbers2Letters(NumberPuzzle, LetterPuzzle).

checkAllCardinals(Puzzle, PuzzleSize) :-
      checkAllCardinals(Puzzle, PuzzleSize, 1, 1).
   
getRandomCardinals(PuzzleSize, Puzzle) :-
        random_permutation([3, 2], [X, Y]) ,
        assert(numberCardinal(X)) , 
        assert(numberCardinal(Y)) ,
        Square is PuzzleSize * PuzzleSize ,
        length(InitialPuzzle, Square) ,
        checkAllCardinals(InitialPuzzle, PuzzleSize) ,
        labeling([], InitialPuzzle) ,
        retract(numberCardinal(2)) , 
        retract(numberCardinal(3)) ,
        numbers2Letters(InitialPuzzle, Puzzle).

:- dynamic numberPole/1.

checkPole(Puzzle, Result, PuzzleSize, Row, Column) :-
        getPuzzleElement(Pole, Result, PuzzleSize, Row, Column) ,
        Pole in -1..1 ,
        numberPole(Pole),
        retract(numberPole(-1)) , 
        retract(numberPole(0)) , 
        retract(numberPole(1)) , 
        random_permutation([-1, 0, 1], [X, Y, Z]) ,
        assert(numberPole(X)) , 
        assert(numberPole(Y)) ,
        assert(numberPole(Z)) ,
        checkRepelling(Result, PuzzleSize, Row, Column) ,    
        getPuzzleElement(Cardinal, Puzzle, PuzzleSize, Row, Column) ,
        getOtherHalf(Row, Column, Cardinal, Row2, Column2, _) ,
        getPuzzleElement(Pole2, Result, PuzzleSize, Row2, Column2) ,
        Pole + Pole2 #= 0.

checkAllPoles(Puzzle, Result, PuzzleSize, Row, Column) :-
        Row > PuzzleSize -> true ;
        (Column > PuzzleSize ->
         (Row1 is Row + 1 ,
          checkAllPoles(Puzzle, Result, PuzzleSize, Row1, 1)) ;
         (checkPole(Puzzle, Result, PuzzleSize, Row, Column) ,
          Column1 is Column + 1 ,
          checkAllPoles(Puzzle, Result, PuzzleSize, Row, Column1))).

checkAllPoles(Puzzle, Result, PuzzleSize) :-
        checkAllPoles(Puzzle, Result, PuzzleSize, 1, 1).

getRandomPoles(Puzzle, Result, PuzzleSize) :-
        random_permutation([-1, 0, 1], [X, Y, Z]) ,
        assert(numberPole(X)) , 
        assert(numberPole(Y)) ,
        assert(numberPole(Z)) ,
        Square is PuzzleSize * PuzzleSize ,
        length(Result, Square) ,
        checkAllPoles(Puzzle, Result, PuzzleSize) ,
        labeling([], Result) ,
        retract(numberPole(-1)) , 
        retract(numberPole(0)) ,
        retract(numberPole(1)).

getElementFrequency([], _,  Result, Counter) :-
        Result = Counter.
getElementFrequency([Current|List], Element, Result, Counter) :-
        (Current == Element ->
         (Counter1 is Counter + 1 ,
          getElementFrequency(List, Element, Result, Counter1)) ;
         getElementFrequency(List, Element, Result, Counter)).
getElementFrequency(List, Element, Result) :-
        getElementFrequency(List, Element, Result, 0).

getListFrequencies([], _, []).
getListFrequencies([List|Lists], Element, [Result|Results]) :-
        getElementFrequency(List, Element, Result),
        getListFrequencies(Lists, Element, Results).

getMinusPlus(_, [], [], [], [], []).        
getMinusPlus(PuzzleSize, Result, Rminus, Rplus, Cminus, Cplus) :-
        unflatten(Result, RowsResult, PuzzleSize) ,
        transpose(RowsResult, ColumnsResult) ,
        getListFrequencies(RowsResult, -1, Rminus) ,
        getListFrequencies(RowsResult, 1, Rplus) ,
        getListFrequencies(ColumnsResult, -1, Cminus) ,
        getListFrequencies(ColumnsResult, 1, Cplus).
               
getRandomPuzzle(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus) :-
        getRandomCardinals(PuzzleSize, Puzzle) ,
        getRandomPoles(Puzzle, Result, PuzzleSize) ,
        getMinusPlus(PuzzleSize, Result, Rminus, Rplus, Cminus, Cplus).
