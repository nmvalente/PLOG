/* -*- Mode:Prolog; coding:utf-8; -*- */

:-consult(utils).

printTopGridRec([_]) :- 
        write('───┐').
printTopGridRec([Element|FirstRow]) :-
        write('───') ,
        (Element == e ->
         write('─') ;
         write('┬')) ,
        printTopGridRec(FirstRow).
printTopGrid(FirstRow) :-
        write('   ┌') ,
        printTopGridRec(FirstRow).

printBottomGridRec([_]) :- 
        write('───┘').
printBottomGridRec([Element|LastRow]) :-
        write('───') ,
        (Element == e ->
         write('─') ;
         write('┴')) ,
        printBottomGridRec(LastRow).
printBottomGrid(LastRow) :-
        write('   └') ,
        printBottomGridRec(LastRow).

printMiddleGridRec([TopElement],_) :- 
        TopElement == s ->
         write('   │') ;
         write('───┤').
printMiddleGridRec([TopElement, NextTopElement|TopRow], [BottomElement|BottomRow]) :-
        (TopElement == e ->
         (write('───') ,       
          (BottomElement == e ->
           write('─') ;
           write('┬'))) ;
         (TopElement == s ->
          (write('   ') ,
           (NextTopElement == s ->
            write('│') ;
            write('├'))) ;
          (write('───') ,       
           BottomElement == e ->
           write('┴') ;
           (NextTopElement == s ->
            write('┤') ;
            write('┼'))))) ,
        printMiddleGridRec([NextTopElement|TopRow], BottomRow).
printMiddleGrid([TopElement|TopRow], BottomRow) :-
         (TopElement == s ->
          write('   │') ;
          write('   ├')) ,
         printMiddleGridRec([TopElement|TopRow], BottomRow).

printPolesRec([_], [Pole]) :-
        (Pole == -1 ->
         write(' - ') ;
         (Pole == 1 ->
          write(' + ') ;
          write(' x '))),
        write('│ ').
printPolesRec([Cardinal|Cardinals], [Pole|Poles]) :-
        (Pole == -1 ->
         write(' - ') ;
         (Pole == 1 ->
          write(' + ') ;
          write(' x '))) ,
        (Cardinal == e ->        
         write(' ') ;
         write('│')) ,
        printPolesRec(Cardinals, Poles).
printPoles(Cardinals, Poles, Rplu, Rminu) :-
        write(' ') ,
        print(Rplu) ,
        write(' │') ,
        printPolesRec(Cardinals, Poles) ,
        print(Rminu).

printTopPlusRec([]).
printTopPlusRec([Element|Cplus]) :-
       write(' ') ,
       print(Element) ,
       write('  ') ,
       printTopPlusRec(Cplus).
printTopPlus(Cplus) :-
        write(' +  ') ,
        printTopPlusRec(Cplus).  

printBottomMinusRec([]).
printBottomMinusRec([Element|Cminus]) :-
       write(' ') ,
       print(Element) ,
       write('  ') ,
       printBottomMinusRec(Cminus).
printBottomMinus(Cminus) :-
        write('    ') ,
        printTopPlusRec(Cminus) ,                   
        write(' -').

printResultRec([LastCardinals], [LastPoles], [Rplu], [Rminu]) :-
        printPoles(LastCardinals, LastPoles, Rplu, Rminu) ,
        nl ,
        printBottomGrid(LastCardinals).
printResultRec([ThisCardinals, NextCardinals | RowsPuzzle], [ThisPoles|RowsResult], [Rplu|Rplus], [Rminu|Rminus]) :-
        printPoles(ThisCardinals, ThisPoles, Rplu, Rminu) ,
        nl ,
        printMiddleGrid(ThisCardinals, NextCardinals) ,
        nl ,
        printResultRec([NextCardinals | RowsPuzzle], RowsResult, Rplus, Rminus).
printResult([FirstCardinals|RowsPuzzle], RowsResult, Rplus, Rminus, Cplus, Cminus) :-
        printTopPlus(Cplus) ,
        nl ,
        printTopGrid(FirstCardinals) ,
        nl ,
        printResultRec([FirstCardinals|RowsPuzzle], RowsResult, Rplus, Rminus) ,
        nl ,
        printBottomMinus(Cminus).

printCellsRec([_]) :-
        write('   ') ,
        write('│ ').
printCellsRec([Cardinal|Cardinals]) :-
        write('   ') ,
        (Cardinal == e ->        
         write(' ') ;
         write('│')) ,
        printCellsRec(Cardinals).
printCells(Cardinals, Rplu, Rminu) :-
        write(' ') ,
        print(Rplu) ,
        write(' │') ,
        printCellsRec(Cardinals) ,
        print(Rminu).

printPuzzleRec([LastCardinals], [Rplu], [Rminu]) :-
        printCells(LastCardinals, Rplu, Rminu) ,
        nl ,
        printBottomGrid(LastCardinals).
printPuzzleRec([ThisCardinals, NextCardinals | RowsPuzzle], [Rplu|Rplus], [Rminu|Rminus]) :-
        printCells(ThisCardinals, Rplu, Rminu) ,
        nl ,
        printMiddleGrid(ThisCardinals, NextCardinals) ,
        nl ,
        printPuzzleRec([NextCardinals | RowsPuzzle], Rplus, Rminus).
printPuzzle([FirstCardinals|RowsPuzzle], Rplus, Rminus, Cplus, Cminus) :-
        printTopPlus(Cplus) ,
        nl ,
        printTopGrid(FirstCardinals) ,
        nl ,
        printPuzzleRec([FirstCardinals|RowsPuzzle], Rplus, Rminus) ,
        nl ,
        printBottomMinus(Cminus).



/************/
/* examples */
/************/

printResultE1 :-
        e1Puzzle(Puzzle) , 
        e1Result(Result) ,
        e1PuzzleSize(PuzzleSize) ,
        e1Rminus(Rminus) ,
        e1Rplus(Rplus) ,
        e1Cminus(Cminus) ,
        e1Cplus(Cplus) ,
        unflatten(Puzzle, RowsPuzzle, PuzzleSize) ,                                  
        unflatten(Result, RowsResult, PuzzleSize) ,
        printResult(RowsPuzzle, RowsResult, Rplus, Rminus, Cplus, Cminus).

printResultE2 :-
        e2Puzzle(Puzzle) , 
        e2Result(Result) ,
        e2PuzzleSize(PuzzleSize) ,
        e2Rminus(Rminus) ,
        e2Rplus(Rplus) ,
        e2Cminus(Cminus) ,
        e2Cplus(Cplus) ,
        unflatten(Puzzle, RowsPuzzle, PuzzleSize) ,                                  
        unflatten(Result, RowsResult, PuzzleSize) ,
        printResult(RowsPuzzle, RowsResult, Rplus, Rminus, Cplus, Cminus).

printPuzzleE1 :-
        e1Puzzle(Puzzle) , 
        e1PuzzleSize(PuzzleSize) ,
        e1Rminus(Rminus) ,
        e1Rplus(Rplus) ,
        e1Cminus(Cminus) ,
        e1Cplus(Cplus) ,
        unflatten(Puzzle, RowsPuzzle, PuzzleSize) ,                                  
        printPuzzle(RowsPuzzle, Rplus, Rminus, Cplus, Cminus).

printPuzzleE2 :-
        e2Puzzle(Puzzle) , 
        e2PuzzleSize(PuzzleSize) ,
        e2Rminus(Rminus) ,
        e2Rplus(Rplus) ,
        e2Cminus(Cminus) ,
        e2Cplus(Cplus) ,
        unflatten(Puzzle, RowsPuzzle, PuzzleSize) ,                                  
        printPuzzle(RowsPuzzle, Rplus, Rminus, Cplus, Cminus).
