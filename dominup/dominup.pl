/* -*- Mode:Prolog; coding:utf-8; -*- */


/****************/
/* useful stuff */
/****************/
not(P) :- (call(P) -> fail ; true).


/***************/
/* print board */
/***************/

printBoard :- nl, printLetters, nl, printGridTop, nl, printRows(1), printLetters, nl, nl,!.

printRows(25) :- !.
printRows(X) :- printLeftNumbers(X), printCells(X, 1), printRightNumbers(X), nl, printGrid(X), X1 is X + 1, nl, printRows(X1).

printCells(_,25) :- !.
printCells(X, Y) :- getTopLevel(X, Y, L), 
                     (L > 0 -> halfPiece(X, Y, L, N, C), print(N), printCardinal(C), print(L); write('   ')), 
                     write('│'), Y1 is Y + 1, printCells(X, Y1).

getTopLevel(X, Y, 0) :- not(halfPiece(X, Y, _, _, _)).
getTopLevel(X, Y, L) :- halfPiece(X, Y, L, _, _), L1 is L + 1, not(halfPiece(X, Y, L1, _, _)).

printCardinal(n) :- write('↑').
printCardinal(e) :- write('→').
printCardinal(s) :- write('↓').
printCardinal(w) :- write('←').

printLetters :- write('    A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   Y   X').

printLeftNumbers(X) :- ((X < 10, write(0), write(X), write('│')) ; (write(X), write('│'))).

printRightNumbers(X) :- ((X < 10, write(0), write(X)) ; (write(X))).

printGridTop :- write('  ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐').

printGrid(X) :-((X < 24, 
                write('  ├───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┤'));
                write('  └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘')).


/****************/
/* print player */
/****************/

printPlayer(I) :- write('                   '), printPieces(I, 0, 0, 0, 0, 0, 0, 0, 0).

printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B, N2B) :- (C == 8 -> (nl, (R == 1 -> (write('      Player '), print(I), write('     ')) ; write('                   ')), C1 is 0, R1 is R + 1, printPieces(I, C1, R1, N1T, N2T, N1N, N2N, N1B, N2B)) ;
    ((R == 0 -> (N1T > 7 -> ! ; (N2T > 7 -> (N2T1 is 0, N1T1 is N1T + 1, printPieces(I, C, R, N1T1, N2T1, N1N, N2N, N1B, N2B)) ; ((piece(N1T, N2T, I, P) -> C1 is C + 1, printPieceTop(P) ; C1 is C), N2T1 is N2T + 1, printPieces(I, C1, R, N1T, N2T1, N1N, N2N, N1B, N2B)))) ; 
      ((R == 1 -> (N1N > 7 -> ! ; (N2N > 7 -> (N2N1 is 0, N1N1 is N1N + 1, printPieces(I, C, R, N1T, N2T, N1N1, N2N1, N1B, N2B)) ; ((piece(N1N, N2N, I, P) -> C1 is C + 1, printPieceNumber(N1N, N2N, P) ; C1 is C), N2N1 is N2N + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N1, N1B, N2B)))) ;
        ((R == 2 -> (N1B > 7 -> ! ; (N2B > 7 -> (N2B1 is 0, N1B1 is N1B + 1, printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B1, N2B1)) ; ((piece(N1B, N2B, I, P) -> C1 is C + 1, printPieceBottom(P) ; C1 is C), N2B1 is N2B + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N, N1B, N2B1)))) ;
          ((R == 3 -> (N1T > 7 -> ! ; (N2T > 7 -> (N2T1 is 0, N1T1 is N1T + 1, printPieces(I, C, R, N1T1, N2T1, N1N, N2N, N1B, N2B)) ; ((piece(N1T, N2T, I, P) -> C1 is C + 1, printPieceTop(P) ; C1 is C), N2T1 is N2T + 1, printPieces(I, C1, R, N1T, N2T1, N1N, N2N, N1B, N2B)))) ; 
            ((R == 4 -> (N1N > 7 -> ! ; (N2N > 7 -> (N2N1 is 0, N1N1 is N1N + 1, printPieces(I, C, R, N1T, N2T, N1N1, N2N1, N1B, N2B)) ; ((piece(N1N, N2N, I, P) -> C1 is C + 1, printPieceNumber(N1N, N2N, P) ; C1 is C), N2N1 is N2N + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N1, N1B, N2B)))) ;
              ((R == 5 -> (N1B > 7 -> ! ; (N2B > 7 -> (N2B1 is 0, N1B1 is N1B + 1, printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B1, N2B1)) ; ((piece(N1B, N2B, I, P) -> C1 is C + 1, printPieceBottom(P) ; C1 is C), N2B1 is N2B + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N, N1B, N2B1)))) ; 
                !))))))))))))).

printPieceTop(P) :- (P == 0 -> write(' ┌───┬───┐') ; write('          ')).

printPieceNumber(N1, N2, P) :- (P == 0 -> write(' │ '), print(N1), write(' │ '), print(N2), write(' │') ; write('          ')).

printPieceBottom(P) :- (P == 0 -> write(' └───┴───┘') ; write('          ')).


/*piece(Number1, Number2, Player, Played).*/

piece(0, 1, 1, 0). 
piece(0, 3, 1, 0). 
piece(0, 4, 1, 0). 
piece(0, 6, 1, 0). 
piece(1, 1, 1, 1). 
piece(1, 2, 1, 0). 
piece(1, 4, 1, 0). 
piece(1, 5, 1, 0). 
piece(1, 7, 1, 0). 
piece(2, 2, 1, 0). 
piece(2, 3, 1, 1). 
piece(2, 6, 1, 0). 
piece(3, 3, 1, 1). 
piece(3, 7, 1, 0). 
piece(4, 5, 1, 0). 
piece(5, 6, 1, 0). 
piece(6, 7, 1, 0).
piece(7, 7, 1, 1).

piece(0, 0, 2, 0).
piece(0, 2, 2, 0). 
piece(0, 5, 2, 1). 
piece(0, 7, 2, 0). 
piece(1, 3, 2, 0). 
piece(1, 6, 2, 0). 
piece(2, 4, 2, 1). 
piece(2, 5, 2, 1). 
piece(2, 7, 2, 0). 
piece(3, 4, 2, 0). 
piece(3, 5, 2, 1). 
piece(3, 6, 2, 0). 
piece(4, 4, 2, 0). 
piece(4, 6, 2, 0). 
piece(4, 7, 2, 1). 
piece(5, 5, 2, 0). 
piece(5, 7, 2, 0). 
piece(6, 6, 2, 1).


/*halfPiece(Line, Column, Level, Number, Cardinal).*/

halfPiece(12, 11, 1, 1, s).
halfPiece(13, 11, 1, 1, n).
halfPiece(12, 12, 1, 7, e).
halfPiece(12, 13, 1, 7, w).
halfPiece(13, 13, 2, 4, n).
halfPiece(12, 13, 2, 7, s).
halfPiece(14, 13, 1, 2, n).
halfPiece(13, 13, 1, 4, s).
halfPiece(15, 13, 1, 3, e).
halfPiece(15, 14, 1, 3, w).
halfPiece(14, 13, 2, 2, s).
halfPiece(15, 13, 2, 3, n).
halfPiece(15, 14, 2, 3, n).
halfPiece(14, 14, 2, 5, s).
halfPiece(14, 13, 3, 2, e).
halfPiece(14, 14, 3, 5, w).
halfPiece(14, 15, 1, 0, w).
halfPiece(14, 14, 1, 5, e).
halfPiece(14, 16, 1, 6, s).
halfPiece(15, 16, 1, 6, n).


