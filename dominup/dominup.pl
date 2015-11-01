/* -*- Mode:Prolog; coding:utf-8; -*- */


/*************/
/* libraries */
/*************/

:- use_module(library(system)).
:- use_module(library(random)).


/****************/
/* useful stuff */
/****************/

not(P) :- (P -> fail ; true).


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

printPlayer(I) :- write('         '), printPieces(I, 0, 0, 0, 0, 0, 0, 0, 0).

printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B, N2B) :- 
        (C == 9 -> (nl, (R == 1 -> (write(' Player '), print(I)) ; write('         ')), 
                    C1 is 0, R1 is R + 1, printPieces(I, C1, R1, N1T, N2T, N1N, N2N, N1B, N2B)) ; 
         ((R == 0 -> (N1T > 7 -> ! ; 
                      (N2T > 7 -> (N2T1 is N1T + 1, N1T1 is N1T + 1, printPieces(I, C, R, N1T1, N2T1, N1N, N2N, N1B, N2B)) ; 
                       ((piece(N1T, N2T, I, P) -> C1 is C + 1, printPieceTop(P) ; C1 is C), 
                        N2T1 is N2T + 1, printPieces(I, C1, R, N1T, N2T1, N1N, N2N, N1B, N2B)))) ; 
           ((R == 1 -> (N1N > 7 -> ! ; 
                        (N2N > 7 -> (N2N1 is N1N + 1, N1N1 is N1N + 1, printPieces(I, C, R, N1T, N2T, N1N1, N2N1, N1B, N2B)) ; 
                         ((piece(N1N, N2N, I, P) -> C1 is C + 1, printPieceNumber(N1N, N2N, P) ; C1 is C), 
                          N2N1 is N2N + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N1, N1B, N2B)))) ;
             ((R == 2 -> (N1B > 7 -> ! ; 
                          (N2B > 7 -> (N2B1 is N1B + 1, N1B1 is N1B + 1, printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B1, N2B1)) ; 
                           ((piece(N1B, N2B, I, P) -> C1 is C + 1, printPieceBottom(P) ; C1 is C), 
                            N2B1 is N2B + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N, N1B, N2B1)))) ;
               ((R == 3 -> (N1T > 7 -> ! ; 
                            (N2T > 7 -> (N2T1 is N1T + 1, N1T1 is N1T + 1, printPieces(I, C, R, N1T1, N2T1, N1N, N2N, N1B, N2B)) ; 
                             ((piece(N1T, N2T, I, P) -> C1 is C + 1, printPieceTop(P) ; C1 is C), 
                              N2T1 is N2T + 1, printPieces(I, C1, R, N1T, N2T1, N1N, N2N, N1B, N2B)))) ; 
                 ((R == 4 -> (N1N > 7 -> ! ; 
                              (N2N > 7 -> (N2N1 is N1N + 1, N1N1 is N1N + 1, printPieces(I, C, R, N1T, N2T, N1N1, N2N1, N1B, N2B)) ; 
                               ((piece(N1N, N2N, I, P) -> C1 is C + 1, printPieceNumber(N1N, N2N, P) ; C1 is C), 
                                N2N1 is N2N + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N1, N1B, N2B)))) ;
                   ((R == 5 -> (N1B > 7 -> ! ; 
                                (N2B > 7 -> (N2B1 is N1B + 1, N1B1 is N1B + 1, printPieces(I, C, R, N1T, N2T, N1N, N2N, N1B1, N2B1)) ; 
                                 ((piece(N1B, N2B, I, P) -> C1 is C + 1, printPieceBottom(P) ; C1 is C), 
                                  N2B1 is N2B + 1, printPieces(I, C1, R, N1T, N2T, N1N, N2N, N1B, N2B1)))) ; 
                     !))))))))))))).

printPieceTop(P) :- (P == 0 -> write(' ┌───┬───┐') ; write('          ')).

printPieceNumber(N1, N2, P) :- (P == 0 -> write(' │ '), print(N1), write(' │ '), print(N2), write(' │') ; write('          ')).

printPieceBottom(P) :- (P == 0 -> write(' └───┴───┘') ; write('          ')).


/*********************/
/* distribute pieces */
/*********************/

:- volatile piece/4.
:- dynamic piece/4.
/* piece(Number1, Number2, Player, Played). */

seedRandom :- now(B), X is B mod 30268 + 1, Y is B mod 30306 + 1, Z is B mod 30322 + 1, setrand(random(X, Y, Z, B)).
seedRandom.

maybeRandom :- random(1, 3, I), (I == 1 -> true; fail). 

distributePieces(N1, N2, NI1, NI2) :- (N1 >=  7 -> assert(piece(N1, N2, 1, 0)) ; 
                                       (N2 > 7 -> (N21 is N1 + 1, N11 is N1 + 1, distributePieces(N11, N21, NI1, NI2)) ;
                                        (NI1 == 17 -> assert(piece(N1, N2, 2, 0)), NI21 is NI2 + 1, N21 is N2 + 1, distributePieces(N1, N21, NI1, NI21) ;
                                         (NI2 == 18 -> assert(piece(N1, N2, 1, 0)), NI11 is NI1 + 1, N21 is N2 + 1, distributePieces(N1, N21, NI11, NI2) ;
                                          (maybeRandom -> (assert(piece(N1, N2, 1, 0)), NI11 is NI1 + 1, N21 is N2 + 1, distributePieces(N1, N21, NI11, NI2)) ; 
                                           (assert(piece(N1, N2, 2, 0)), NI21 is NI2 + 1, N21 is N2 + 1, distributePieces(N1, N21, NI1, NI21))))))).

testDistribute :-
        assert(piece(0, 1, 1, 0)),
        assert(piece(0, 3, 1, 0)), 
        assert(piece(0, 4, 1, 0)), 
        assert(piece(0, 6, 1, 0)), 
        assert(piece(1, 1, 1, 0)), 
        assert(piece(1, 2, 1, 0)), 
        assert(piece(1, 4, 1, 0)), 
        assert(piece(1, 5, 1, 0)), 
        assert(piece(1, 7, 1, 0)), 
        assert(piece(2, 2, 1, 0)), 
        assert(piece(2, 3, 1, 0)), 
        assert(piece(2, 6, 1, 0)), 
        assert(piece(3, 3, 1, 0)), 
        assert(piece(3, 7, 1, 0)), 
        assert(piece(4, 5, 1, 0)), 
        assert(piece(5, 6, 1, 0)), 
        assert(piece(6, 7, 1, 0)),
        assert(piece(7, 7, 1, 0)),
        assert(piece(0, 0, 2, 0)),
        assert(piece(0, 2, 2, 0)), 
        assert(piece(0, 5, 2, 0)), 
        assert(piece(0, 7, 2, 0)), 
        assert(piece(1, 3, 2, 0)), 
        assert(piece(1, 6, 2, 0)), 
        assert(piece(2, 4, 2, 0)), 
        assert(piece(2, 5, 2, 0)), 
        assert(piece(2, 7, 2, 0)), 
        assert(piece(3, 4, 2, 0)), 
        assert(piece(3, 5, 2, 0)), 
        assert(piece(3, 6, 2, 0)), 
        assert(piece(4, 4, 2, 0)), 
        assert(piece(4, 6, 2, 0)), 
        assert(piece(4, 7, 2, 0)), 
        assert(piece(5, 5, 2, 0)), 
        assert(piece(5, 7, 2, 0)), 
        assert(piece(6, 6, 2, 0)).


/**************/
/* play piece */
/**************/

:- volatile halfPiece/5.
:- dynamic halfPiece/5.
/* halfPiece(Line, Column, Level, Number, Cardinal). */

getOtherHalf(X1, Y1, n, X2, Y2, s) :- X2 is X1 - 1, Y2 is Y1. 
getOtherHalf(X1, Y1, e, X2, Y2, w) :- X2 is X1, Y2 is Y1 + 1. 
getOtherHalf(X1, Y1, s, X2, Y2, n) :- X2 is X1 + 1, Y2 is Y1. 
getOtherHalf(X1, Y1, w, X2, Y2, e) :- X2 is X1, Y2 is Y1 - 1. 

playPiece(N1, N2, I, X, Y, C) :- getTopLevel(X, Y, L), L1 is L + 1, assert(halfPiece(X, Y, L1, N1, C)), 
        getOtherHalf(X, Y, C, X2, Y2, C2), assert(halfPiece(X2, Y2, L1, N2, C2)),
        retract(piece(N1, N2, I, 0)), assert(piece(N1, N2, I, 1)).

testPlay :-
        playPiece(7, 7, 1, 12, 12, e),
        playPiece(2, 4, 2, 14, 13, n),
        playPiece(3, 3, 1, 15, 13, e),
        playPiece(4, 7, 2, 13, 13, n),
        playPiece(0, 5, 2, 14, 15, w),
        playPiece(2, 3, 1, 14, 13, s),
        playPiece(1, 1, 1, 12, 11, s),
        playPiece(3, 5, 2, 15, 14, n),
        playPiece(2, 5, 2, 14, 13, e),
        playPiece(6, 6, 2, 14, 16, s).

/*
halfPiece(12, 12, 1, 7, e).
halfPiece(12, 13, 1, 7, w).
halfPiece(12, 11, 1, 1, s).
halfPiece(13, 11, 1, 1, n).
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
*/

