
/* 
the lists of lists (rows of columns) are flattened,
for example, [[a, b], [c, d]] is represented as [a, b, c, d].
*/

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


/*************/
/* libraries */
/*************/

:- set_prolog_flag(toplevel_print_options, [quoted(true), portrayed(true), max_depth(0)]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(random)).


/*************/
/* utilities */
/*************/

/* computes the seed for the random number generator in sicstus */
seedRandom :- 
        now(B) , 
        X is B mod 30268 + 1 , 
        Y is B mod 30306 + 1 , 
        Z is B mod 30322 + 1 , 
        setrand(random(X, Y, Z, B)).
:- seedRandom.


/************/
/* solution */
/************/

getPuzzleElement(Element, Puzzle, PuzzleSize, Row, Column) :-
        Position is (Row - 1) * PuzzleSize + Column , 
        nth1(Position, Puzzle, Element).

/* predicate used to obtain the position and cardinal of the other 
   half of a magnet, given the position and cardinal of one half */
getOtherHalf(Row1, Column1, n, Row2, Column2, s) :- 
        Row2 is Row1 - 1 , Column2 is Column1. 
getOtherHalf(Row1, Column1, e, Row2, Column2, w) :- 
        Row2 is Row1 , Column2 is Column1 + 1. 
getOtherHalf(Row1, Column1, s, Row2, Column2, n) :- 
        Row2 is Row1 + 1 , Column2 is Column1. 
getOtherHalf(Row1, Column1, w, Row2, Column2, e) :- 
        Row2 is Row1 , Column2 is Column1 - 1.

getRowColumn(Position, Size, Row, Column) :-
        Row is div(Position, Size) + 1 ,
        Column is mod(Position, Size).

checkMagnet(Puzzle, Result, PuzzleSize, Line, Column) :-
        getPuzzleElement(Cardinal, Puzzle, PuzzleSize, Line, Column) ,
        (member(Cardinal, [w, n]) -> true ;
         (getOtherHalf(Line, Column, Cardinal, Line2, Column2, _) ,
          getPuzzleElement(Pole1, Result, PuzzleSize, Line, Column) ,
          getPuzzleElement(Pole2, Result, PuzzleSize, Line2, Column2) ,
          Pole1 + Pole2 #= 0)).

checkAllMagnets(Puzzle, Result, PuzzleSize, Line, Column) :-
        Line > PuzzleSize -> true ; 
        (Column > PuzzleSize -> 
         (Line1 is Line + 1 , 
          checkAllMagnets(Puzzle, Result, PuzzleSize, Line1, 1)) ;
         (checkMagnet(Puzzle, Result, PuzzleSize, Line, Column) ,
         Column1 is Column + 1 ,
         checkAllMagnets(Puzzle, Result, PuzzleSize, Line, Column1))).
         
checkAllMagnets(Puzzle, Result, PuzzleSize) :-
        checkAllMagnets(Puzzle, Result, PuzzleSize, 1, 1).

getRCzeros([], _, [], []).
getRCzeros([RCzero|RCzeros], PuzzleSize, [RCminu|RCminus], [RCplu|RCplus]) :-
        RCzero is PuzzleSize - (RCminu + RCplu) ,
        getRCzeros(RCzeros, PuzzleSize, RCminus, RCplus).

getFrequencies([], [], [], []).
getFrequencies([[-1-Mfreq, 0-Zfreq, 1-Pfreq]|RCfrequencies], [RCminu|RCminus], [RCzero|RCzeros], [RCplu|RCplus]) :-
        Mfreq is RCminu , Zfreq is RCzero , Pfreq is RCplu ,
        getFrequencies(RCfrequencies, RCminus, RCzeros, RCplus).        

checkFrequency([], []).
checkFrequency([RC|RCs], [RCfrequency|RCfrequencies]) :-
        global_cardinality(RC, RCfrequency) ,
        checkFrequency(RCs, RCfrequencies).

unflatten([], [], _, _).
unflatten([Element|Puzzle], [[Element|Row]|RowsPuzzle], PuzzleSize, Counter) :-
        Counter >= PuzzleSize -> 
        (Row = [] ,
         unflatten(Puzzle, RowsPuzzle, PuzzleSize, 1)) ;
        (Counter1 is Counter + 1 ,
         unflatten(Puzzle, [Row|RowsPuzzle], PuzzleSize, Counter1)).
unflatten(Puzzle, RowsPuzzle, PuzzleSize) :- 
        unflatten(Puzzle, RowsPuzzle, PuzzleSize, 1).
        
checkAllFrequencies(Result, PuzzleSize, Rminus, Rplus, Cminus, Cplus) :-
        unflatten(Result, ResultRows, PuzzleSize) ,
        transpose(ResultRows, ResultColumns) ,
        getRCzeros(Rzeros, PuzzleSize, Rminus, Rplus) ,
        getRCzeros(Czeros, PuzzleSize, Cminus, Cplus) ,
        getFrequencies(Rfrequencies, Rminus, Rzeros, Rplus) ,
        getFrequencies(Cfrequencies, Cminus, Czeros, Cplus) ,
        checkFrequency(ResultRows, Rfrequencies) ,
        checkFrequency(ResultColumns, Cfrequencies).        

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

checkRepelling(Result, PuzzleSize, Row, Column) :-
        getPuzzleElement(Pole, Result, PuzzleSize, Row, Column) ,
        getEastSouth(Result, PuzzleSize, Row, Column, East, South) ,
        Pole + East #< 2 ,
        Pole + East #> -2 ,
        Pole + South #< 2 ,
        Pole + South #> -2 .

checkAllRepelling(Result, PuzzleSize, Row, Column) :-
        Row > PuzzleSize -> true ;
        (Column > PuzzleSize ->
         (Row1 is Row + 1 ,
          checkAllRepelling(Result, PuzzleSize, Row1, 1)) ;
         (checkRepelling(Result, PuzzleSize, Row, Column) ,
          Column1 is Column + 1 ,
          checkAllRepelling(Result, PuzzleSize, Row, Column1))).

checkAllRepelling(Result, PuzzleSize) :-
        checkAllRepelling(Result, PuzzleSize, 1, 1).

solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus) :-
        Square is PuzzleSize * PuzzleSize ,
        length(Result, Square) ,
        checkAllFrequencies(Result, PuzzleSize, Rminus, Rplus, Cminus, Cplus) ,
        checkAllMagnets(Puzzle, Result, PuzzleSize) ,
        checkAllRepelling(Result, PuzzleSize) ,
        labeling([ff, median], Result).

/**************/
/* generation */
/**************/

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

solveE1(E1Result) :-
        e1Puzzle(Puzzle) , 
        e1PuzzleSize(PuzzleSize) ,
        e1Rminus(Rminus) ,
        e1Rplus(Rplus) ,
        e1Cminus(Cminus) ,
        e1Cplus(Cplus) ,
        solvePuzzle(E1Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus).

solveE2(E2Result) :-
        e2Puzzle(Puzzle) , 
        e2PuzzleSize(PuzzleSize) ,
        e2Rminus(Rminus) ,
        e2Rplus(Rplus) ,
        e2Cminus(Cminus) ,
        e2Cplus(Cplus) ,
        solvePuzzle(E2Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus).        


/**************/
/* statistics */
/**************/

generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Time) :-
        statistics(walltime, [Start,_]) ,
        getRandomPuzzle(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus) ,
        statistics(walltime, [End,_]) ,
        Time is End - Start.

solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, Time) :-
        statistics(walltime, [Start,_]) ,
        solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus) ,
        statistics(walltime, [End,_]) ,
        Time is End - Start.
        
run(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen) ,
        solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, TimeSol) , 
        format('Puzzle of size ~d generated in ~3d seconds and solved in ~3d seconds.~n', [PuzzleSize, TimeGen, TimeSol]).

run(PuzzleSize) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen) ,
        solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, TimeSol) , 
        format('Puzzle of size ~d generated in ~3d seconds and solved in ~3d seconds.~n', [PuzzleSize, TimeGen, TimeSol]).

runAll(PuzzleSize) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen) ,
        statistics(walltime, [Start,_]) ,
        findall(Result, solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus), List) ,
        statistics(walltime, [End,_]) ,
        TimeSol is End - Start ,
        length(List, N) ,
        TimeAvg is TimeSol / N ,
        format('Puzzle of size ~d generated in ~3d seconds, ~d solutions found in ~3d seconds, ', [PuzzleSize, TimeGen, N, TimeSol]) ,
        format('average time per solution is ~3d seconds.~n', [TimeAvg]).

runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, Counter, TimeGen, TimeSol) :-
        Counter > PuzzleNumber -> 
        (TotalTimeGen = TimeGen ,
         TotalTimeSol = TimeSol) ;
        (generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Time1) ,
         TimeGen1 is TimeGen + Time1 ,
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time2) , 
         TimeSol1 is TimeSol + Time2 ,
         Counter1 is Counter + 1 ,
         runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, Counter1, TimeGen1, TimeSol1)).

run(PuzzleSize, PuzzleNumber) :-
        runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, 1, 0, 0) ,
        TimeGen is TotalTimeGen / PuzzleNumber ,
        TimeSol is TotalTimeSol / PuzzleNumber ,
        format('~d puzzles of size ~d generated in an average time of ~3d seconds', [PuzzleNumber, PuzzleSize, TimeGen]) ,
        format('and solved in an average time of ~3d seconds.~n', [TimeSol]).
