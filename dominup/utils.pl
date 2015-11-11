/* -*- Mode:Prolog; coding:utf-8; -*- */


/*************/
/* libraries */
/*************/

:- use_module(library(system)).
:- use_module(library(random)).


/****************/
/* useful stuff */
/****************/

/* not predicate */
not(P) :- 
        (P -> fail ; true).

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

/* predicate used to get a single algarism from the input */
/* does not require full dot at the end, 
   removes all other characters up to a new line,
   works also if user only presses enter
   but the number will be -38 */
getDigit(D) :- 
        get_code(Dt) , D is Dt - 48 , (Dt == 10 -> ! ; getNewLine).

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

/* predicate used to get a list of chars from the input */
/* does not require full dot at the end, 
   removes the new line character,
   the result is placed on OutList,
   InList should be the empty list */
getCharList(InList, OutList) :- 
        get_char(C) , 
        char_code(C, Co) , 
        ( Co == 10 -> 
          OutList = InList ; 
          append(InList, [C], InList1), 
          getCharList(InList1, OutList)).

/* places the first N elemenst of the list L on the list P */
trim(L, N, P) :- 
        length(L, M) , 
        (N >= M -> 
         P = L ; 
         (X is M - N , 
          length(S, X) , 
          append(P, S, L))).

/* gets an atom with at most 8 characters from the input */
/* does not require full dot at the end, 
   removes the new line character */
get8String(S) :- 
        getCharList([], InList) , 
        trim(InList, 8, OutList) , 
        atom_chars(S, OutList).

/* gets an atom with any number of characters from the input */
/* does not require full dot at the end, 
   removes the new line character */
getString(S) :- 
        getCharList([], OutList) , 
        atom_chars(S, OutList).

/* clears the screen on unix consoles */        
cls :- write('\e[H\e[J\e[3J').


/**********/
/* random */
/**********/

/* computes the seed for the random number generator in sicstus */
seedRandom :- 
        now(B) , 
        X is B mod 30268 + 1 , 
        Y is B mod 30306 + 1 , 
        Z is B mod 30322 + 1 , 
        setrand(random(X, Y, Z, B)).
:- seedRandom.

/* suceeds with 1/2 probability, fails in other cases */
maybeRandom :- 
        random(1, 3, I) , 
        (I == 1 -> true ; fail). 
