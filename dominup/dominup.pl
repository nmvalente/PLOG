/* -*- Mode:Prolog; coding:utf-8; -*- */

/*********************/
/* distribute pieces */
/*********************/

:- dynamic piece/4.
/* piece(Number1, Number2, Player, Played). */

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

:- dynamic halfPiece/5.
/* halfPiece(Line, Column, Level, Number, Cardinal). */

getOtherHalf(X1, Y1, n, X2, Y2, s) :- X2 is X1 - 1, Y2 is Y1. 
getOtherHalf(X1, Y1, e, X2, Y2, w) :- X2 is X1 , Y2 is Y1 + 1. 
getOtherHalf(X1, Y1, s, X2, Y2, n) :- X2 is X1 + 1, Y2 is Y1. 
getOtherHalf(X1, Y1, w, X2, Y2, e) :- X2 is X1 , Y2 is Y1 - 1.

:- dynamic playPiece/6.
/* playPiece(Number1, Number2, Player, Line, Column, Cardinal). */

playPiece(N1, N2, I, X1, Y1, C1, L) :- checkPlay(N1, N2, I, X1, Y1, C1, X2, Y2, C2, L) -> (L1 is L + 1 , 
                                                             assert(halfPiece(X1, Y1, L1, N1, C1)) , 
                                                             assert(halfPiece(X2, Y2, L1, N2, C2)) ,
                                                             retract(piece(N1, N2, I, 0)) , assert(piece(N1, N2, I, 1))) ; fail.

checkPlay(N1, N2, I, X1, Y1, C1, X2, Y2, C2, L) :- getTopLevel(X1, Y1, L) , getOtherHalf(X1, Y1, C1, X2, Y2, C2) , 
        checkInsideBoard(X1, Y1, X2, Y2) , checkPlayerPiece(N1, N2, I) , checkLevelStable(L, X2, Y2) , 
        (L == 0 -> checkNoClimbs(I) , checkExpandOrthogonal(X1, Y1, C1, X2, Y2, C2) ; checkClimbNumbers(N1, X1, Y1, N2, X2, Y2, L)).

checkInsideBoard(X1, Y1, X2, Y2) :- check1InsideBoard(X1) , check1InsideBoard(Y1) , check1InsideBoard(X2) , check1InsideBoard(Y2).

check1InsideBoard(Z) :- Z < 1 -> fail ; (Z > 18 -> fail ; true).

checkPlayerPiece(N1, N2, I) :- piece(N1, N2, I, 0).

checkLevelStable(L, X2, Y2) :- getTopLevel(X2, Y2, L).

checkClimbNumbers(N1, X1, Y1, N2, X2, Y2, L) :- halfPiece(X1, Y1, L, N1, _) , halfPiece(X2, Y2, L, N2, _).

checkExpandOrthogonal(X1, Y1, C1, X2, Y2, C2) :- checkHalfOrthogonal(X1, Y1, C1) ; checkHalfOrthogonal(X2, Y2, C2).

checkHalfOrthogonal(X, Y, C) :- checkNorthOrthogonal(X, Y, C) ; checkEastOrthogonal(X, Y, C) ; checkSouthOrthogonal(X, Y, C) ; checkWestOrthogonal(X, Y, C).

checkNorthOrthogonal(X, Y, C) :- X1 is X - 1 , (member(C, [e, w]) -> halfPiece(X1, Y, 1, _, n) ;
                                                (C == s -> (halfPiece(X1, Y, 1, _, e) ; halfPiece(X1, Y, 1, _, w)) ; fail)).

checkEastOrthogonal(X, Y, C) :- Y1 is Y + 1 , (member(C, [s, n]) -> halfPiece(X, Y1, 1, _, e) ;
                                               (C == w -> (halfPiece(X, Y1, 1, _, n) ; halfPiece(X, Y1, 1, _, s)) ; fail)).

checkSouthOrthogonal(X, Y, C) :- X1 is X + 1 , (member(C, [w, e]) -> halfPiece(X1, Y, 1, _, s) ;
                                                (C == n -> (halfPiece(X1, Y, 1, _, e) ; halfPiece(X1, Y, 1, _, w)) ; fail)).

checkWestOrthogonal(X, Y, C) :- Y1 is Y - 1 , (member(C, [n, s]) -> halfPiece(X, Y1, 1, _, w) ;
                                               (C == e -> (halfPiece(X, Y1, 1, _, n) ; halfPiece(X, Y1, 1, _, s)) ; fail)).

checkNoClimbs(I) :- getClimbPlays(I, Plays) , length(Plays, Length) , Length == 0.

testPlay :- playFirstPiece ,
        playPiece(2, 4, 2, 11, 10, n, _) ,
        playPiece(3, 3, 1, 12, 10, e, _) ,
        playPiece(4, 7, 2, 10, 10, n, _) ,
        playPiece(0, 5, 2, 11, 12, w, _) ,
        playPiece(2, 3, 1, 11, 10, s, _) ,
        playPiece(1, 1, 1, 9, 8, s, _) ,
        playPiece(3, 5, 2, 12, 11, n, _) ,
        playPiece(2, 5, 2, 11, 10, e, _) ,
        playPiece(6, 6, 2, 11, 13, s, _) .

test :- testDistribute , testPlay.

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

checkGameOver :- numberPieces(1, 0, 0, 0, R1), (R1 == 0 -> (printBoard , write('Game Over: Player 1 wins!'), fail) ; 
                                                (numberPieces(2, 0, 0, 0, R2), (R2 == 0 -> (printBoard , write('Game Over: Player 2 wins!'), fail) ; true))).

playFirstPiece :- assert(halfPiece(9, 9, 1, 7, e)), assert(halfPiece(9, 10, 1, 7, w)), retract(piece(7, 7, 1, 0)) , assert(piece(7, 7, 1, 1)).

playGame :- distributePieces(0, 0, 0, 0) , playFirstPiece , assert(turn(2)) , playTurn.

playTurn :- checkGameOver -> (turn(I) , player(I, _, T) , 
                              (T == 1 -> (printGame(I) , getMove(I, N1, N2, X1, Y1, C1) , 
                                          (playPiece(N1, N2, I, X1, Y1, C1, L) -> (nextPlayer(I, L) , playTurn) ; 
                                           (write('Invalid movement.') , sleep(1) , playTurn))) ;
                               (T == 2 -> (playRandom(I) , changeTurn(I) , playTurn) ; (playBest(I), changeTurn(I) , playTurn)))) ; !.

getMove(I, N1, N2, X1, Y1, C1) :- nl , getContinue , getN1(I, N1) , getN2(I, N1, N2) , getX1(X1) , getY1(Y1) , getC1(C1).

getContinue :- prompt(_, 'Continue - enter , Save and exit - s: ') , getChar(C) , (C == 's' -> save_game ; ! ).

getN1(I, N1) :- prompt(_, 'Piece left number: ') , getDigit(N1t) ,
        (piece(N1t, _, I, 0) -> N1 is N1t ; 
         (write('You have no piece ') , print(N1t) , write(' │ ? .') , nl ,  getN1(I, N1))).

getN2(I, N1, N2) :- prompt(_, 'Piece right number: ') , getDigit(N2t) , 
        (piece(N1, N2t, I, 0) -> N2 is N2t ; 
         (write('You have no piece ') , print(N1) , write(' │ ') , print(N2t) , write(' .') , nl , getN2(I, N1, N2))). 

getX1(X1) :- prompt(_, 'Line for left number (1 - 18): ') , getDoubleDigit(X1t) , 
        (X1t < 1 -> (write('Line number must be at least 1.') , nl ,  getX1(X1)) ;
         (X1t > 18 -> (write('Line number must be at most 18.') , nl , getX1(X1)) ;
          X1 is X1t)).

getY1(Y1) :- prompt(_, 'Column for left number (1 - 18): ') , getDoubleDigit(Y1t) , 
        (Y1t < 1 -> (write('Column number must be at least 1.') , nl , getY1(Y1)) ;
         (Y1t > 18 -> (write('Column number must be at most 18.') , nl , getY1(Y1)) ;
          Y1 is Y1t)).

getC1(C1) :- prompt(_, 'Cardinal of right number relative to left number: ') , getChar(C1t) ,
        (member(C1t, [n, e, s, w]) -> copy_term(C1t, C1) ; 
         (write('Cardinal must be one of: n, e, s, w.'), nl , getC1(C1))).

:- dynamic turn/1.

changeTurn(I) :- I1 is 3 - I , retract(turn(I)) , assert(turn(I1)).

nextPlayer(I, L) :- L == 0 -> changeTurn(I) ; !.


/**************************/
/* artifical intelligence */
/**************************/

:- dynamic position/2.

getPositions(X, Y) :- X > 18 -> ! ; 
                      (Y > 18 -> (Y1 is 1 , X1 is X + 1 , getPositions(X1, Y1)) ;
                       (assert(position(X, Y)), Y1 is Y + 1, getPositions(X, Y1))).
:- getPositions(1, 1).

checkClimb(N1, N2, I, X1, Y1, C1) :- checkPlayerPiece(N1, N2, I) , getTopLevel(X1, Y1, L) , getOtherHalf(X1, Y1, C1, X2, Y2, _) , 
        checkInsideBoard(X1, Y1, X2, Y2) , checkLevelStable(L, X2, Y2) , checkClimbNumbers(N1, X1, Y1, N2, X2, Y2, L).

checkExpand(N1, N2, I, X1, Y1, C1) :- checkPlayerPiece(N1, N2, I) , position(X1, Y1), getOtherHalf(X1, Y1, C1, X2, Y2, C2) , 
        checkInsideBoard(X1, Y1, X2, Y2) , checkLevelStable(0, X1, Y1), checkLevelStable(0, X2, Y2) , checkExpandOrthogonal(X1, Y1, C1, X2, Y2, C2).

getClimbPlays(I, Plays) :- findall(play(N1, N2, I, X1, Y1, C1), checkClimb(N1, N2, I, X1, Y1, C1), Plays).

randomClimbPlay(I) :- getClimbPlays(I, Plays) , random_member(play(N1, N2, I, X1, Y1, C1), Plays) , playPiece(N1, N2, I, X1, Y1, C1, _).

getExpandPlays(I, Plays) :- findall(play(N1, N2, I, X1, Y1, C1), checkExpand(N1, N2, I, X1, Y1, C1), Plays).

randomExpandPlay(I) :- getExpandPlays(I, Plays) , random_member(play(N1, N2, I, X1, Y1, C1), Plays) , playPiece(N1, N2, I, X1, Y1, C1, _).

playRandom(I) :- randomClimbPlay(I) -> playRandom(I) ; randomExpandPlay(I).

evaluateSituation(I, R) :- getNumberPlays(I, Rme) , I1 is 3 - I , getNumberPlays(I1 , Ryou) , R is Rme - Ryou.

getNumberPlays(I, R) :- getClimbPlays(I, Plays), length(Plays, S) , R is S + 1.

playPieceNoCheck(N1, N2, I, X1, Y1, C1, L) :- getOtherHalf(X1, Y1, C1, X2, Y2, C2) , getTopLevel(X1, Y1, L) , L1 is L + 1 , 
        assert(halfPiece(X1, Y1, L1, N1, C1)) , assert(halfPiece(X2, Y2, L1, N2, C2)) , retract(piece(N1, N2, I, 0)) , assert(piece(N1, N2, I, 1)).

removePiece(N1, N2, I, X1, Y1, C1, L) :- getOtherHalf(X1, Y1, C1, X2, Y2, C2) , getTopLevel(X1, Y1, L) , 
        retract(halfPiece(X1, Y1, L, N1, C1)) , retract(halfPiece(X2, Y2, L, N2, C2)) , assert(piece(N1, N2, I, 0)) , retract(piece(N1, N2, I, 1)).

evaluateClimb(N1, N2, I, X1, Y1, C1, R) :- checkClimb(N1, N2, I, X1, Y1, C1) , playPieceNoCheck(N1, N2, I, X1, Y1, C1, _) , 
        evaluateSituation(I, R) , removePiece(N1, N2, I, X1, Y1, C1, _).

evaluateExpand(N1, N2, I, X1, Y1, C1, R) :- checkExpand(N1, N2, I, X1, Y1, C1) , playPieceNoCheck(N1, N2, I, X1, Y1, C1, _) , 
        evaluateSituation(I, R) , removePiece(N1, N2, I, X1, Y1, C1, _).

evaluateClimbPlays(I, Plays) :- findall(play(N1, N2, I, X1, Y1, C1, R), evaluateClimb(N1, N2, I, X1, Y1, C1, R), Plays).

evaluateExpandPlays(I, Plays) :- findall(play(N1, N2, I, X1, Y1, C1, R), evaluateExpand(N1, N2, I, X1, Y1, C1, R), Plays).

bestPlay([], _) :- fail.
bestPlay([Play|Plays], MPlay) :- bestPlay(Plays, Play, MPlay).
bestPlay([], play(N1, N2, I, X1, Y1, C1, R), play(N1, N2, I, X1, Y1, C1, R)).
bestPlay([play(N11, N21, I1, X11, Y11, C11, R1)|Plays] , play(N12, N22, I2, X12, Y12, C12, R2) , play(MN1, MN2, MI, MX1, MY1, MC1, MR)) :- 
        (R1 > R2 -> bestPlay(Plays, play(N11, N21, I1, X11, Y11, C11, R1), play(MN1, MN2, MI, MX1, MY1, MC1, MR)) ;
         bestPlay(Plays, play(N12, N22, I2, X12, Y12, C12, R2), play(MN1, MN2, MI, MX1, MY1, MC1, MR))).

bestClimbPlay(I) :- evaluateClimbPlays(I, Plays) , bestPlay(Plays, play(N1, N2, I, X1, Y1, C1, _)) , playPiece(N1, N2, I, X1, Y1, C1, _).

bestExpandPlay(I) :- evaluateExpandPlays(I, Plays) , bestPlay(Plays, play(N1, N2, I, X1, Y1, C1, _)) , playPiece(N1, N2, I, X1, Y1, C1, _).

playBest(I) :- bestClimbPlay(I) -> playBest(I) ; bestExpandPlay(I).
