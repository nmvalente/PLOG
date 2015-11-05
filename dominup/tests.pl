/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- dynamic piece/4.
testDistribute :-
        assert(piece(0, 1, 1, 0)),
        assert(piece(0, 3, 1, 0)), 
        assert(piece(0, 4, 1, 0)), 
        assert(piece(0, 6, 1, 0)), 
        assert(piece(1, 1, 1, 0)), 
        assert(piece(1, 2, 1, 0)), 
        assert(piece(1, 4, 1, 0)).

cancelSomeDistribute :- retract(piece(0, 1, 1, 0)),
                        retract(piece(0, 3, 1, 0)), 
                        retract(piece(0, 4, 1, 0)).

save_game(File):-save_program(File).

load_game(File):-restore(File).

