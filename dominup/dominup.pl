/* -*- Mode:Prolog; coding:utf-8; -*- */

/* useful stuff */
not(P) :- (call(P) -> fail ; true).

/* board display */
displayBoard :- nl, putLetters, nl, putGridTop, nl, row(1), putLetters, nl, nl,!.

row(N) :- ((N < 25, putLeftNumbers(N), analyseCell(N, 1), putRightNumbers(N), nl, putGrid(N), N1 is N + 1, nl, row(N1)) ; !).

analyseCell(_, 25) :- !.
analyseCell(25, _) :- !.
analyseCell(X, Y) :- displayCell(X, Y),  write('│'), Y1 is Y + 1, analyseCell(X, Y1).

displayCell(X, Y) :- getTopLevel(X, Y, L), (L > 0 -> halfPiece(X, Y, L, N, C), print(N), displayCardinal(C), print(L); write('   ')).

getTopLevel(X, Y, 0) :- not(halfPiece(X, Y, _, _, _)).
getTopLevel(X, Y, L) :- halfPiece(X, Y, L, _, _), L1 is L + 1, not(halfPiece(X, Y, L1, _, _)).

displayCardinal(n) :- write('↑').
displayCardinal(e) :- write('→').
displayCardinal(s) :- write('↓').
displayCardinal(w) :- write('←').

putLetters :- write('    A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   Y   X').

putLeftNumbers(N) :- ((N < 10, write(0), write(N), write('│')) ; (write(N), write('│'))).

putRightNumbers(N) :- ((N < 10, write(0), write(N)) ; (write(N))).

putGridTop :- write('  ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐').

putGrid(N) :-((N < 24, 
              write('  ├───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┤'));
             (write('  └───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───┘'))).



/*piece(Line, Column, Level, Number1, Number2, Cardinal).*/
/*
piece(12, 11, 1, 1, 1, s).
piece(12, 12, 1, 7, 7, e).
piece(13, 13, 2, 4, 7, n).
piece(14, 13, 1, 2, 4, n).
piece(14, 13, 2, 2, 3, s).
piece(14, 15, 1, 0, 5, w).
piece(14, 13, 3, 2, 5, e).
piece(15, 14, 2, 3, 5, n).
piece(15, 13, 1, 3, 3, e).
piece(14, 16, 1, 6, 6, s).
*/
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


