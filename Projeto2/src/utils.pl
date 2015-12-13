
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

/* predicate used to remove all characters up to a new line from the input */
getNewLine :- 
        get_code(T) , (T == 10 -> ! ; getNewLine).

/* predicate used to get a single char from the input */
/* does not require full dot at the end, 
   removes all other characters up to a new line,
   works also if user only presses enter */
getChar(C) :- 
        get_char(C) , char_code(C, Co) , (Co == 10 -> ! ; getNewLine).

/* predicate used to get a possible double algarism number from the input */
/* does not require full dot at the end, 
   removes all other characters up to a new line,
   works also if user only presses enter
   but the number will be -38 */
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
