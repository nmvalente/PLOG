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

printBoard :- nl , printNumbers , nl , printGridTop , nl , printRows(1) , printNumbers , nl , nl , !.

printRows(25) :- !.
printRows(X) :- printLeftNumbers(X) , printCells(X, 1) , printRightNumbers(X) , nl , printGrid(X) , X1 is X + 1 , nl , printRows(X1).

printCells(_,25) :- !.
printCells(X, Y) :- getTopLevel(X, Y, L) , 
                     (L > 0 -> (halfPiece(X, Y, L, N, C) , print(N) , printCardinal(C) , print(L)) ; write('   ')) , 
                     write('│') , Y1 is Y + 1 , printCells(X, Y1).

getTopLevel(X, Y, 0) :- not(halfPiece(X, Y, _, _, _)).
getTopLevel(X, Y, L) :- halfPiece(X, Y, L, _, _) , L1 is L + 1 , not(halfPiece(X, Y, L1, _, _)).

printCardinal(n) :- write('↑').
printCardinal(e) :- write('→').
printCardinal(s) :- write('↓').
printCardinal(w) :- write('←').

printNumbers :- write('    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24').

printLeftNumbers(X) :- ((X < 10, write(' ') , write(X) , write('│')) ; (write(X) , write('│'))).

printRightNumbers(X) :- ((X < 10 , write(' '), write(X)) ; (write(X))).

printGridTop :- write('  ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐').

printGrid(X) :-((X < 24 , 
                write('  ├───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┤')) ;
                write('  └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘')).


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

printGame(I) :- printBoard, printPlayer(I).


/*********************/
/* distribute pieces */
/*********************/

:- volatile piece/4.
:- dynamic piece/4.
/* piece(Number1, Number2, Player, Played). */

seedRandom :- now(B) , X is B mod 30268 + 1 , Y is B mod 30306 + 1 , Z is B mod 30322 + 1 , setrand(random(X, Y, Z, B)).
seedRandom.

maybeRandom :- random(1, 3, I) , (I == 1 -> true ; fail). 

distributePieces(N1, N2, NI1, NI2) :- (N1 >=  7 -> assert(piece(N1, N2, 1, 0)) ; 
                                       (N2 > 7 -> (N21 is N1 + 1 , N11 is N1 + 1 , distributePieces(N11, N21, NI1, NI2)) ;
                                        (NI1 == 17 -> assert(piece(N1, N2, 2, 0)) , NI21 is NI2 + 1 , N21 is N2 + 1 , distributePieces(N1, N21, NI1, NI21) ;
                                         (NI2 == 18 -> assert(piece(N1, N2, 1, 0)) , NI11 is NI1 + 1 , N21 is N2 + 1 , distributePieces(N1, N21, NI11, NI2) ;
                                          (maybeRandom -> (assert(piece(N1, N2, 1, 0)) , NI11 is NI1 + 1 , N21 is N2 + 1 , distributePieces(N1, N21, NI11, NI2)) ; 
                                           (assert(piece(N1, N2, 2, 0)) , NI21 is NI2 + 1, N21 is N2 + 1 , distributePieces(N1, N21, NI1, NI21))))))).

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
getOtherHalf(X1, Y1, e, X2, Y2, w) :- X2 is X1 , Y2 is Y1 + 1. 
getOtherHalf(X1, Y1, s, X2, Y2, n) :- X2 is X1 + 1, Y2 is Y1. 
getOtherHalf(X1, Y1, w, X2, Y2, e) :- X2 is X1 , Y2 is Y1 - 1.

:- volatile playPiece/6.
:- dynamic playPiece/6.
/* playPiece(Number1, Number2, Player, Line, Column, Cardinal). */

playPiece(N1, N2, I, X1, Y1, C1, L) :- getOtherHalf(X1, Y1, C1, X2, Y2, C2) , getTopLevel(X1, Y1, L), 
        checkPlay(N1, N2, I, X1, Y1, C1, X2, Y2, C2, L) , L1 is L + 1, 
        assert(halfPiece(X1, Y1, L1, N1, C1)), 
        assert(halfPiece(X2, Y2, L1, N2, C2)),
        retract(piece(N1, N2, I, 0)) , assert(piece(N1, N2, I, 1)).

checkPlay(N1, N2, I, X1, Y1, C1, X2, Y2, C2, L) :- checkInsideBoard(X1, Y1, X2, Y2) , checkPlayerPiece(N1, N2, I) , checkLevelStable(L, X2, Y2), 
        (L == 0 -> checkExpandOrthogonal(X1, Y1, C1, X2, Y2, C2) ; checkClimbNumbers(N1, X1, Y1, N2, X2, Y2, L)).

checkInsideBoard(X1, Y1, X2, Y2) :- check1InsideBoard(X1) , check1InsideBoard(Y1) , check1InsideBoard(X2) , check1InsideBoard(Y2).

check1InsideBoard(Z) :- Z < 1 -> fail ; (Z > 24 -> fail ; true).

checkPlayerPiece(N1, N2, I) :- piece(N1, N2, I, 0).

checkLevelStable(L, X2, Y2) :- getTopLevel(X2, Y2, L).

checkClimbNumbers(N1, X1, Y1, N2, X2, Y2, L) :- halfPiece(X1, Y1, L, N1, _) , halfPiece(X2, Y2, L, N2, _).

checkExpandOrthogonal(X1, Y1, C1, X2, Y2, C2) :- checkHalfOrthogonal(X1, Y1, C1) ; checkHalfOrthogonal(X2, Y2, C2).

checkHalfOrthogonal(X, Y, C) :- checkNorthOrthogonal(X, Y, C) ; checkEastOrthogonal(X, Y, C) ; checkSouthOrthogonal(X, Y, C) ; checkWestOrthogonal(X, Y, C).

/* halfPiece(Line, Column, Level, Number, Cardinal). */
checkNorthOrthogonal(X, Y, C) :- C == e -> halfPiece(X - 1, Y, 1, _, n) ;
                                 (C == s -> (halfPiece(X - 1, Y, 1, _, e) ; halfPiece(X - 1, Y, 1, _, w)) ;
                                  (C == w -> halfPiece(X - 1, Y, 1, _, n) ; fail)).

checkEastOrthogonal(X, Y, C) :- C == s -> halfPiece(X, Y + 1, 1, _, e) ;
                                (C == w -> (halfPiece(X, Y + 1, 1, _, n) ; halfPiece(X, Y + 1, 1, _, s)) ;
                                 (C == n -> halfPiece(X, Y + 1, 1, _, e) ; fail)).

checkSouthOrthogonal(X, Y, C) :- C == w -> halfPiece(X + 1, Y, 1, _, s) ;
                                 (C == n -> (halfPiece(X + 1, Y, 1, _, e) ; halfPiece(X + 1, Y, 1, _, w)) ;
                                  (C == e -> halfPiece(X + 1, Y, 1, _, s) ; fail)).

checkWestOrthogonal(X, Y, C) :- C == n -> halfPiece(X, Y - 1, 1, _, w) ;
                                (C == e -> (halfPiece(X, Y - 1, 1, _, n) ; halfPiece(X, Y - 1, 1, _, s)) ;
                                 (C == s -> halfPiece(X, Y - 1, 1, _, w) ; fail)).

testPlay :-
        playPiece(7, 7, 1, 12, 12, e, _),
        playPiece(2, 4, 2, 14, 13, n, _),
        playPiece(3, 3, 1, 15, 13, e, _),
        playPiece(4, 7, 2, 13, 13, n, _),
        playPiece(0, 5, 2, 14, 15, w, _),
        playPiece(2, 3, 1, 14, 13, s, _),
        playPiece(1, 1, 1, 12, 11, s, _),
        playPiece(3, 5, 2, 15, 14, n, _),
        playPiece(2, 5, 2, 14, 13, e, _),
        playPiece(6, 6, 2, 14, 16, s, _).

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


/***************/
/* game engine */
/***************/

numberPieces(I, NP, N1, N2, R) :- N1 > 7 -> R is NP ; (N2 > 7 -> (N21 is N1 + 1 , N11 is N1 + 1 , numberPieces(I, NP, N11, N21, R)) ; 
                                              (piece(N1, N2, I, 0) -> (NP1 is NP + 1 , N21 is N2 + 1, numberPieces(I, NP1, N1, N21, R)) ; 
                                               (N21 is N2 + 1 , numberPieces(I, NP, N1, N21, R)))).

checkGameOver(R) :- numberPieces(1, 0, 0, 0, R1), (R1 == 0 -> R is 1 ; (numberPieces(2, 0, 0, 0, R2), (R2 == 0 -> R is 2 ; R is 0))).

printResult(R) :- write('Game Over: Player '), print(R), write(' wins!').

startGame :- distributePieces(0, 0, 0, 0) , playFirstPiece.

playFirstPiece :- assert(halfPiece(12, 12, 1, 7, e)), assert(halfPiece(12, 13, 1, 7, w)), retract(piece(7, 7, 1, 0)) , assert(piece(7, 7, 1, 1)).

playGame :- startGame , printGame(2) , playTurn(2).

playTurn(I) :- checkGameOver(R) , (R == 0 -> (getMove(I, N1, N2, X1, Y1, C1) , 
                                              (playPiece(N1, N2, I, X1, Y1, C1, L) -> (nextPlayer(I, I1, L) , printGame(I1), playTurn(I1)) ; (write('Invalid movement.') , playTurn(I))); 
                                              printResult(R))).

getMove(I, N1, N2, X1, Y1, C1) :- nl , getN1(I, N1) , getN2(I, N1, N2) , getX1(X1) , getY1(Y1) , getC1(C1).

getN1(I, N1) :- write('Piece left number: ') , read(N1t) , 
        (piece(N1t, _, I, 0) -> N1 is N1t ; 
         (write('You have no piece ') , print(N1t) , write(' │ ? .') , nl ,  getN1(I, N1))).

getN2(I, N1, N2) :- write('Piece right number: ') , read(N2t) , 
        (piece(N1, N2t, I, 0) -> N2 is N2t ; 
         (write('You have no piece ') , print(N1) , write(' │ ') , print(N2t) , write(' .') , nl , getN2(I, N1, N2))). 

getX1(X1) :- write('Line for left number (1 - 24): ') , read(X1t) , 
        (X1t < 1 -> (write('Line number must be at least 1.') , nl ,  getX1(X1)) ;
         (X1t > 24 -> (write('Line number must be at most 24.') , nl , getX1(X1)) ;
          X1 is X1t)).

getY1(Y1) :- write('Column for left number (1 - 24): ') , read(Y1t) , 
        (Y1t < 1 -> (write('Column number must be at least 1.') , nl , getY1(Y1)) ;
         (Y1t > 24 -> (write('Column number must be at most 24.') , nl , getY1(Y1)) ;
          Y1 is Y1t)).

getC1(C1) :- write('Cardinal of right number relative to left number: ') , read(C1t) ,
        (member(C1t, [n, e, s, w]) -> copy_term(C1t, C1) ; 
         (write('Cardinal must be one of: n, e, s, w.'), nl , getC1(C1))).

nextPlayer(I, I1, L) :- L == 0 -> I1 is 3 - I ; I1 is I.

