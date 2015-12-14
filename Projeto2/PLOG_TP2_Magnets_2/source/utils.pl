/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/* this file contains some useful functions */

:- set_prolog_flag(toplevel_print_options, [quoted(true), portrayed(true), max_depth(0)]).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(random)).

/* computes the seed for the random number generator in sicstus */
seedRandom :- 
        now(B) , 
        X is B mod 30268 + 1 , 
        Y is B mod 30306 + 1 , 
        Z is B mod 30322 + 1 , 
        setrand(random(X, Y, Z, B)).
:- seedRandom.

even(N) :- 
        N rem 2 =:= 0.

/* removes all characters up to a new line from the input */
getNewLine :- 
        get_code(T) , (T == 10 -> ! ; getNewLine).

/* gets a single char from the input */
getChar(C) :- 
        get_char(C) , char_code(C, Co) , (Co == 10 -> ! ; getNewLine).

/* gets a possible double algarism number from the input */
getDoubleDigit(D) :- 
        get_code(D1t) , 
        (D1t == 10 -> ! ; 
         (get_code(D2t) , 
          (D2t == 10 -> 
           (D is D1t - 48) ; 
           (getNewLine , 
            D is (D1t - 48) * 10 + D2t - 48)))).

/* clears the screen on unix consoles */        
cls :- write('\e[H\e[J\e[3J').
