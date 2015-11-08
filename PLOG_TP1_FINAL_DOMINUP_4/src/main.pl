/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

/*************/
/* libraries */
/*************/

:- use_module(library(system)).
:- use_module(library(random)).
:- use_module(library(file_systems)).

:-consult(display).
:-consult(dominup).


/****************/
/* useful stuff */
/****************/

/* not predicate */
not(P) :- 
        (P -> fail ; true).

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


/************/
/* start up */
/************/

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

/* asks the user if they want to play a new game or load a saved game */
newOrLoad(Option) :- 
        prompt(_, '1 - New Game , 2 - Load Game: ') ,
        getDigit(Optiont) , 
        (Optiont < 1 -> (write('Must be 1 or 2.') , nl , newOrLoad(Option)) ; 
         (Optiont > 2 -> (write('Must be 1 or 2.') , nl , newOrLoad(Option)) ;
          Option is Optiont)).

:- dynamic player/3.
/* player (Number, Name, Type) */
/* Number is 1 or 2, randomly decided at the start of a new game */
/* Name is chosen by player if human, otherwise is Computer or Compute1 or Compute2 */
/* Type is 1 if Human, 2 if Easy, 3 if Hard */
        
/* asks the user if about the type of game (human vs human, human vs computer, computer vs computer) */
computerOrHuman(Option) :- 
        prompt(_, '1 - Human vs Human, 2 - Human vs Computer, 3 - Computer vs Computer : ') ,
        getDigit(Optiont) , 
        (Optiont < 1 -> (write('Must be 1, 2 or 3.') , nl , computerOrHuman(Option)) ; 
         (Optiont > 3 -> (write('Must be 1, 2 or 2.') , nl , computerOrHuman(Option)) ;
          Option is Optiont)).

/* asks the user if the computer artificial intelligence should be easy (random) or hard (greedy) */
easyOrHard(Option) :- prompt(_, '1 - Easy, 2 - Hard : ') , 
        getDigit(Optiont) , (Optiont < 1 -> (write('Must be 1 or 2.') , nl , computerOrHuman(Option)) ; 
                             (Optiont > 2 -> (write('Must be 1 or 2.') , nl , computerOrHuman(Option)) ;
                              Option is Optiont)).

/* asks the user for human player names and randomly chooses player numbers, creating the player predicates */
playerNames(CHOption, EHOption1, EHOption2) :- 
        CHOption == 1 ->
        (prompt(_, 'Name of one player: ') , 
         get8String(X1) , 
         (X1 = '' -> X = 'Player 1' ; X = X1),  
         prompt(_, 'Name of the other player: ') , 
         get8String(Y1) , 
         (Y1 = '' -> Y = 'Player 2' ; Y = Y1) , 
         nl , write('Hello ') ,  write(X) , write(' and ' ) , write(Y) , write('!') , nl ,
         (maybeRandom -> 
          (assert(player(1, X, 1)) , assert(player(2, Y, 1))) ; 
          (assert(player(1, Y, 1)) , assert(player(2, X, 1))))) ;
        (CHOption == 2 ->
         (prompt(_, 'Player name: ') , 
          get8String(X1) , 
          (X1 = '' -> X = 'Player' ; X = X1) , 
          nl , write('Hello ') , write(X) , write('!') , nl , 
          Y = 'Computer' ,
          (maybeRandom -> 
           (assert(player(1, X, 1)) , assert(player(2, Y, EHOption1))) ; 
           (assert(player(1, Y, EHOption1)) , assert(player(2, X, 1))))) ;
         (assert(player(1, 'Compute1', EHOption1)) , 
          assert(player(2, 'Compute2', EHOption2)))).

/* saves the current state of the game and stops it */
save_game :- 
        prompt(_, 'File name: ') , 
        getString(S) , 
        save_program(S) , 
        break.

/* loads a game state from a file */
load_game :- 
        prompt(_, 'File name: ') , 
        getString(S) , 
        atom_concat(S, '.sav', Ssav) , 
        (file_exists(Ssav, exist) -> 
         restore(S) ; 
         (print(S) , write(' is not a valid save file') , nl , load_game)).
      
/* starts a game */
/* if the game has been loaded proceeds to play the next turn
   otherwise, asks the user for initial setup and then calls playGame*/
startGame :- player(1, _, _) -> playTurn ;
             (cls , nl , write('Welcome to Dominup!!!') , nl , nl , newOrLoad(NLOption) , 
              (NLOption == 1 -> 
               (computerOrHuman(CHOption) ,
                (CHOption == 1 -> (playerNames(CHOption, _, _)) ;
                 (CHOption == 2 -> (easyOrHard(EHOptiont) , EHOption is EHOptiont + 1 , playerNames(CHOption, EHOption, _)) ;
                  (write('Choose level for Computer 1.') , nl , easyOrHard(EHOptiont1) , EHOption1 is EHOptiont1 + 1 ,
                   write('Choose level for Computer 2.') , nl , easyOrHard(EHOptiont2) , EHOption2 is EHOptiont2 + 1 ,
                   playerNames(CHOption, EHOption1, EHOption2) , prompt(_, 'Enter to continue.'))))) , sleep(1) , playGame) ;
               load_game).

/* always begin by calling startGame */
:- initialization(startGame).
