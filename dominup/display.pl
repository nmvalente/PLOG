/* -*- Mode:Prolog; coding:utf-8; -*- */

 
/***************/
/* print board */
/***************/

printBoard :- nl , printNumbers , nl , printGridTop , nl , printRows(1) , printNumbers , nl , nl , !.

printRows(19) :- !.
printRows(X) :- printLeftNumbers(X) , printCells(X, 1) , printRightNumbers(X) , nl , printGrid(X) , X1 is X + 1 , nl , printRows(X1).

printCells(_,19) :- !.
printCells(X, Y) :- getTopLevel(X, Y, L) , 
                     (L > 0 -> (halfPiece(X, Y, L, N, C) , print(N) , printCardinal(C) , print(L)) ; write('   ')) , 
                     write('│') , Y1 is Y + 1 , printCells(X, Y1).

getTopLevel(X, Y, 0) :- not(halfPiece(X, Y, _, _, _)).
getTopLevel(X, Y, L) :- halfPiece(X, Y, L, _, _) , L1 is L + 1 , not(halfPiece(X, Y, L1, _, _)).

printCardinal(n) :- write('↑').
printCardinal(e) :- write('→').
printCardinal(s) :- write('↓').
printCardinal(w) :- write('←').

printNumbers :- write('                  1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18').

printLeftNumbers(X) :- ((X < 10, write('               ') , print(X) , write('│')) ; (write('              ') , print(X) , write('│'))).

printRightNumbers(X) :- ((X < 10 , write(' '), print(X)) ; (print(X))).

printGridTop :- write('                ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐').

printGrid(X) :-((X < 18 , 
                write('                ├───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┤')) ;
                write('                └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘')).


/****************/
/* print player */
/****************/

printPlayer(I) :- write('         ') , printPieces(I, 0, 0, 0, 0, 0, 0, 0, 0).

printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B, N2B) :- 
        (C == 9 -> (nl , (R == 1 -> (write(' Player ') , print(I)) ; write('         ')) , 
                    C1 is 0 , R1 is R + 1 , printPieces(I, C1, R1, N1T, N2T, N1N, N2N, N1B, N2B)) ; 
         ((R == 0 -> (N1T > 7 -> ! ; 
                      (N2T > 7 -> (N2T1 is N1T + 1 , N1T1 is N1T + 1 , printPieces(I, C, R, N1T1, N2T1, N1N, N2N, N1B, N2B)) ; 
                       ((piece(N1T, N2T, I, P) -> C1 is C + 1 , printPieceTop(P) ; C1 is C), 
                        N2T1 is N2T + 1 , printPieces(I, C1, R, N1T, N2T1, N1N, N2N, N1B, N2B)))) ; 
           ((R == 1 -> (N1N > 7 -> ! ; 
                        (N2N > 7 -> (N2N1 is N1N + 1 , N1N1 is N1N + 1 , printPieces(I, C, R, N1T, N2T, N1N1, N2N1, N1B, N2B)) ; 
                         ((piece(N1N, N2N, I, P) -> C1 is C + 1 , printPieceNumber(N1N, N2N, P) ; C1 is C), 
                          N2N1 is N2N + 1 , printPieces(I, C1, R, N1T, N2T, N1N, N2N1, N1B, N2B)))) ;
             ((R == 2 -> (N1B > 7 -> ! ; 
                          (N2B > 7 -> (N2B1 is N1B + 1 , N1B1 is N1B + 1 , printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B1, N2B1)) ; 
                           ((piece(N1B, N2B, I, P) -> C1 is C + 1 , printPieceBottom(P) ; C1 is C), 
                            N2B1 is N2B + 1 , printPieces(I, C1, R, N1T, N2T, N1N, N2N, N1B, N2B1)))) ;
               ((R == 3 -> (N1T > 7 -> ! ; 
                            (N2T > 7 -> (N2T1 is N1T + 1 , N1T1 is N1T + 1 , printPieces(I, C, R, N1T1, N2T1, N1N, N2N, N1B, N2B)) ; 
                             ((piece(N1T, N2T, I, P) -> C1 is C + 1 , printPieceTop(P) ; C1 is C), 
                              N2T1 is N2T + 1 , printPieces(I, C1, R, N1T, N2T1, N1N, N2N, N1B, N2B)))) ; 
                 ((R == 4 -> (N1N > 7 -> ! ; 
                              (N2N > 7 -> (N2N1 is N1N + 1 , N1N1 is N1N + 1 , printPieces(I, C, R, N1T, N2T, N1N1, N2N1, N1B, N2B)) ; 
                               ((piece(N1N, N2N, I, P) -> C1 is C + 1 , printPieceNumber(N1N, N2N, P) ; C1 is C), 
                                N2N1 is N2N + 1 , printPieces(I, C1, R, N1T, N2T, N1N, N2N1, N1B, N2B)))) ;
                   ((R == 5 -> (N1B > 7 -> ! ; 
                                (N2B > 7 -> (N2B1 is N1B + 1 , N1B1 is N1B + 1 , printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B1, N2B1)) ; 
                                 ((piece(N1B, N2B, I, P) -> C1 is C + 1 , printPieceBottom(P) ; C1 is C), 
                                  N2B1 is N2B + 1 , printPieces(I, C1, R, N1T, N2T, N1N, N2N, N1B, N2B1)))) ; 
                     !))))))))))))).

printPieceTop(P) :- (P == 0 -> write(' ┌───┬───┐') ; write('          ')).

printPieceNumber(N1, N2, P) :- (P == 0 -> write(' │ ') , print(N1), write(' │ ') , print(N2) , write(' │') ; write('          ')).

printPieceBottom(P) :- (P == 0 -> write(' └───┴───┘') ; write('          ')).


/**************/
/* print game */
/**************/

printGame(I) :- cls , printBoard , printPlayer(I).
