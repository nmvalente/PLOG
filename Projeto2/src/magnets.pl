
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

solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, Options) :-
        Square is PuzzleSize * PuzzleSize ,
        length(Result, Square) ,
        checkAllFrequencies(Result, PuzzleSize, Rminus, Rplus, Cminus, Cplus) ,
        checkAllMagnets(Puzzle, Result, PuzzleSize) ,
        checkAllRepelling(Result, PuzzleSize) ,
        labeling(Options, Result).

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
        solvePuzzle(E1Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, [ff, median]).

solveE2(E2Result) :-
        e2Puzzle(Puzzle) , 
        e2PuzzleSize(PuzzleSize) ,
        e2Rminus(Rminus) ,
        e2Rplus(Rplus) ,
        e2Cminus(Cminus) ,
        e2Cplus(Cplus) ,
        solvePuzzle(E2Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, [ff, median]).        


/**************/
/* statistics */
/**************/

generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Time, Backtracks) :-
        fd_statistics(backtracks, BackStart) ,
        statistics(walltime, [Start,_]) ,
        getRandomPuzzle(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus) ,
        statistics(walltime, [End,_]) ,
        fd_statistics(backtracks, BackEnd) ,
        Time is End - Start ,
        Backtracks is BackEnd - BackStart.

solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, Time, Backtracks, Options) :-
        fd_statistics(backtracks, BackStart) ,
        statistics(walltime, [Start,_]) ,
        solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, Options) ,
        statistics(walltime, [End,_]) ,
        fd_statistics(backtracks, BackEnd) ,
        Time is End - Start ,
        Backtracks is BackEnd - BackStart.
        
run(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, Options) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen, BackGen) ,
        solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, TimeSol, BackSol, Options) , 
        format('Puzzle of size ~d generated in ~3d seconds (~d backtracks) and solved in ~3d seconds (~d backtracks).~n', [PuzzleSize, TimeGen, BackGen, TimeSol, BackSol]).

run(PuzzleSize, Options) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen, BackGen) ,
        solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, TimeSol, BackSol, Options) , 
        format('Puzzle of size ~d generated in ~3d seconds (~d backtracks) and solved in ~3d seconds (~d backtracks).~n', [PuzzleSize, TimeGen, BackGen, TimeSol, BackSol]).

runAll(PuzzleSize, Options) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen, _) ,
        statistics(walltime, [Start,_]) ,
        findall(Result, solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, Options), List) ,
        statistics(walltime, [End,_]) ,
        TimeSol is End - Start ,
        length(List, N) ,
        TimeAvg is TimeSol / N ,
        format('Puzzle of size ~d generated in ~3d seconds, ~d solutions found in ~3d seconds, ', [PuzzleSize, TimeGen, N, TimeSol]) ,
        format('average time per solution is ~3d seconds.~n', [TimeAvg]).

runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, MaxTimeGen, MaxTimeSol, MinTimeGen, MinTimeSol, MaxGen, MaxSol, MinGen, MinSol, Counter, TimeGen, TimeSol, TotalBackGen, TotalBackSol, BackGen, BackSol, Options) :-
        Counter > PuzzleNumber -> 
        (TotalTimeGen = TimeGen ,
         TotalTimeSol = TimeSol , 
         MaxTimeGen = MaxGen , 
         MaxTimeSol = MaxSol , 
         MinTimeGen = MinGen , 
         MinTimeSol = MinSol,
         TotalBackGen = BackGen ,
         TotalBackSol = BackSol) ;
        (generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Time1, Back1) ,
         TimeGen1 is TimeGen + Time1 ,
         BackGen1 is BackGen + Back1 ,
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time2, Back2, Options) , 
         TimeSol1 is TimeSol + Time2 ,
         BackSol1 is BackSol + Back2 ,
         Counter1 is Counter + 1 ,
         (Time1 > MaxGen ->
          MaxGen1 = Time1 ;
          MaxGen1 = MaxGen) ,
         (Time1 < MinGen ->
          MinGen1 = Time1 ;
          MinGen1 = MinGen) ,
         (Time2 > MaxSol ->
          MaxSol1 = Time2 ;
          MaxSol1 = MaxSol) ,
         (Time2 < MinSol ->
          MinSol1 = Time2 ;
          MinSol1 = MinSol) ,
         runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, MaxTimeGen, MaxTimeSol, MinTimeGen, MinTimeSol, MaxGen1, MaxSol1, MinGen1, MinSol1, Counter1, TimeGen1, TimeSol1, TotalBackGen, TotalBackSol, BackGen1, BackSol1, Options)).

run(PuzzleSize, PuzzleNumber, Options, MinGen, AvgGen, MaxGen, BackGen, MinSol, AvgSol, MaxSol, BackSol) :-
        runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, MaxGen, MaxSol, MinGen, MinSol, 0, 0, 3600000000, 3600000000, 1, 0, 0, TotalBackGen, TotalBackSol, 0, 0, Options) ,
        AvgGen is TotalTimeGen / PuzzleNumber ,
        AvgSol is TotalTimeSol / PuzzleNumber ,
        BackGen is TotalBackGen / PuzzleNumber ,
        BackSol is TotalBackSol / PuzzleNumber.

solveAvg(PuzzleSize, PuzzleNumber, Counter ,
         TotalTime_none, MaxTime_none, MinTime_none, Max_none, Min_none, Time_none, TotalBack_none, Back_none,
         TotalTime_ff, MaxTime_ff, MinTime_ff, Max_ff, Min_ff, Time_ff, TotalBack_ff, Back_ff,
         TotalTime_median, MaxTime_median, MinTime_median, Max_median, Min_median, Time_median, TotalBack_median, Back_median,
         TotalTime_all, MaxTime_all, MinTime_all, Max_all, Min_all, Time_all, TotalBack_all, Back_all) :-
        Counter > PuzzleNumber -> 
        (TotalTime_none = Time_none , 
         MaxTime_none = Max_none , 
         MinTime_none = Min_none,
         TotalBack_none = Back_none,
         TotalTime_ff = Time_ff , 
         MaxTime_ff = Max_ff , 
         MinTime_ff = Min_ff,
         TotalBack_ff = Back_ff,
         TotalTime_median = Time_median , 
         MaxTime_median = Max_median , 
         MinTime_median = Min_median,
         TotalBack_median = Back_median,
         TotalTime_all = Time_all , 
         MaxTime_all = Max_all , 
         MinTime_all = Min_all,
         TotalBack_all = Back_all) ;
        (generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, _) ,
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_none, Back0_none, []) , 
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_ff, Back0_ff, [ff]) , 
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_median, Back0_median, [median]) , 
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_all, Back0_all, [ff, median]) , 
         Time1_none is Time_none + Time0_none ,
         Back1_none is Back_none + Back0_none ,
         Time1_ff is Time_ff + Time0_ff ,
         Back1_ff is Back_ff + Back0_ff ,
         Time1_median is Time_median + Time0_median ,
         Back1_median is Back_median + Back0_median ,
         Time1_all is Time_all + Time0_all ,
         Back1_all is Back_all + Back0_all ,
         Counter1 is Counter + 1 ,
         (Time0_none > Max_none ->
          Max1_none = Time0_none;
          Max1_none = Max_none) ,
         (Time0_none < Min_none ->
          Min1_none = Time0_none ;
          Min1_none = Min_none) ,
         (Time0_ff > Max_ff ->
          Max1_ff = Time0_ff;
          Max1_ff = Max_ff) ,
         (Time0_ff < Min_ff ->
          Min1_ff = Time0_ff ;
          Min1_ff = Min_ff) ,
         (Time0_median > Max_median ->
          Max1_median = Time0_median;
          Max1_median = Max_median) ,
         (Time0_median < Min_median ->
          Min1_median = Time0_median ;
          Min1_median = Min_median) ,
         (Time0_all > Max_all ->
          Max1_all = Time0_all;
          Max1_all = Max_all) ,
         (Time0_all < Min_all ->
          Min1_all = Time0_all ;
          Min1_all = Min_all) ,
         solveAvg(PuzzleSize, PuzzleNumber, Counter1 ,
         TotalTime_none, MaxTime_none, MinTime_none, Max1_none, Min1_none, Time1_none, TotalBack_none, Back1_none,
         TotalTime_ff, MaxTime_ff, MinTime_ff, Max1_ff, Min1_ff, Time1_ff, TotalBack_ff, Back1_ff,
         TotalTime_median, MaxTime_median, MinTime_median, Max1_median, Min1_median, Time1_median, TotalBack_median, Back1_median,
         TotalTime_all, MaxTime_all, MinTime_all, Max1_all, Min1_all, Time1_all, TotalBack_all, Back1_all)).

run(PuzzleSize, PuzzleNumber, 
    Avg_none, Max_none, Min_none, Back_none,
    Avg_ff, Max_ff, Min_ff, Back_ff,
    Avg_median, Max_median, Min_median, Back_median,
    Avg_all, Max_all, Min_all, Back_all) :-
        solveAvg(PuzzleSize, PuzzleNumber, 0 ,
                 TotalTime_none, Max_none, Min_none, 0, 3600000000, 0, TotalBack_none, 0,
                 TotalTime_ff, Max_ff, Min_ff, 0, 3600000000, 0, TotalBack_ff, 0,
                 TotalTime_median, Max_median, Min_median, 0, 3600000000, 0, TotalBack_median, 0,
                 TotalTime_all, Max_all, Min_all, 0, 3600000000, 0, TotalBack_all, 0) ,
        Avg_none is TotalTime_none / PuzzleNumber ,
        Avg_ff is TotalTime_ff / PuzzleNumber ,
        Avg_median is TotalTime_median / PuzzleNumber ,
        Avg_all is TotalTime_all / PuzzleNumber ,
        Back_none is TotalBack_none / PuzzleNumber ,
        Back_ff is TotalBack_ff / PuzzleNumber ,
        Back_median is TotalBack_median / PuzzleNumber ,
        Back_all is TotalBack_all / PuzzleNumber.
