/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/*************/
/* libraries */
/*************/

:- use_module(library(system)).
:- use_module(library(random)).


:-consult(display).
:-consult(dominup).

/************/
/* start up */
/************/
load_game(File):-restore(File).

gameOption:-write('1 - Load Game, 2 - New Game'), nl,
            repeat, get_code(Option), Option >= 49, Option =< 50,
            (
                ((Option == 49) -> load_game('test_db.sav')) ;
                ((Option == 50) -> gameType)
            ).

gameType:-
        write('1 - Hum vs PC, 2 - Hum vs Hum, 3 - PC vs PC'), nl,
        repeat, get_code(Option), Option >= 49, Option =< 51,
        type(Option, Jog1, Jog2),
        (      ((Jog1 == 'pc') -> !);
               ((Jog1 == 'human', Jog2 == 'pc') -> (write('Name of one player: '), nl, read(X), write('welcome '), write(X), nl));
               ((Jog2 == 'pc') -> !);
               ((Jog1 == 'human', Jog2 == 'human') -> (write('Name of one player: '), nl, read(X), write('welcome '), write(X), nl,  
                                                      write('Name of other player: '), nl, read(Y), write('welcome '), write(Y), nl))).
        /*:- initialization(playGame).*/
      
type(49, human, pc).
type(50, human, human).
type(51, pc, pc).
