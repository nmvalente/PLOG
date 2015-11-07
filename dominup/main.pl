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

not(P) :- (P -> fail ; true).

getNewLine :- get_code(T) , (T == 10 -> ! ; getNewLine).

getChar(C) :- get_char(C) , char_code(C, Co) , (Co == 10 -> ! ; getNewLine).

getDigit(D) :- get_code(Dt) , D is Dt - 48 , (Dt == 10 -> ! ; getNewLine).

getDoubleDigit(D) :- get_code(D1t) , (D1t == 10 -> ! ; (get_code(D2t) , (D2t == 10 -> (D is D1t - 48) ; (getNewLine , D is (D1t - 48) * 10 + D2t - 48)))).

getCharList(InList, OutList) :- get_char(C) , char_code(C, Co) , ( Co == 10 -> OutList = InList ; append(InList, [C], InList1), getCharList(InList1, OutList)).

trim(L, N, P) :- length(L, M) , (N >= M -> P = L ; (X is M - N , length(S, X) , append(P, S, L))).

get8String(S) :- getCharList([], InList) , trim(InList, 8, OutList) , atom_chars(S, OutList).

getString(S) :- getCharList([], OutList) , atom_chars(S, OutList).
        
cls :- write('\e[H\e[J\e[3J').


/************/
/* start up */
/************/

seedRandom :- now(B) , X is B mod 30268 + 1 , Y is B mod 30306 + 1 , Z is B mod 30322 + 1 , setrand(random(X, Y, Z, B)).
:- seedRandom.

maybeRandom :- random(1, 3, I) , (I == 1 -> true ; fail). 

newOrLoad(Option) :- prompt(_, '1 - New Game , 2 - Load Game: ') ,
        getDigit(Optiont) , (Optiont < 1 -> (write('Must be 1 or 2.') , nl , newOrLoad(Option)) ; 
                             (Optiont > 2 -> (write('Must be 1 or 2.') , nl , newOrLoad(Option)) ;
                              Option is Optiont)).

:- dynamic player/3.
        
computerOrHuman(Option) :- prompt(_, '1 - Player vs Computer, 2 - Player vs Player : ') ,
        getDigit(Optiont) , (Optiont < 1 -> (write('Must be 1 or 2.') , nl , computerOrHuman(Option)) ; 
                             (Optiont > 2 -> (write('Must be 1 or 2.') , nl , computerOrHuman(Option)) ;
                              Option is Optiont)).

easyOrHard(Option) :- prompt(_, '1 - Easy, 2 - Hard : ') , 
        getDigit(Optiont) , (Optiont < 1 -> (write('Must be 1 or 2.') , nl , computerOrHuman(Option)) ; 
                             (Optiont > 2 -> (write('Must be 1 or 2.') , nl , computerOrHuman(Option)) ;
                              Option is Optiont)).

playerNames(CHOption, EHOption) :- CHOption == 1 ->
                                   (prompt(_, 'Player name: ') , get8String(X1) , 
                                    (X1 = '' -> X = 'Player' ; X = X1) , write('Hello ') , write(X) , write('!') , nl , Y = 'Computer' ,
                                    (maybeRandom -> (assert(player(1, X, 1)) , assert(player(2, Y, EHOption))) ; 
                                     (assert(player(1, Y, EHOption)) , assert(player(2, X, 1))))) ;
                                   (prompt(_, 'Name of one player: ') , get8String(X1) , 
                                    (X1 = '' -> X = 'Player 1' ; X = X1) , write('Hello ') , write(X) , write('!') , nl,  
                                    prompt(_, 'Name of the other player: ') , get8String(Y1) , 
                                    (Y1 = '' -> Y = 'Player 2' ; Y = Y1) , write('Hello ') , write(Y) , write('!') , nl ,
                                    (maybeRandom -> (assert(player(1, X, 1)) , assert(player(2, Y, 1))) ; (assert(player(1, Y, 1)) , assert(player(2, X, 1))))).

save_game :- prompt(_, 'File name: ') , getString(S) , save_program(S) , halt.

load_game :- prompt(_, 'File name: ') , getString(S) , atom_concat(S, '.sav', Ssav) , 
        (file_exists(Ssav, exist) -> (restore(S) , playTurn) ; 
         (print(S) , write(' is not a valid save file') , nl , load_game)).
      
startGame :- cls , nl , write('Welcome to Dominup!!!') , nl , nl , newOrLoad(NLOption) , 
        (NLOption == 1 -> (computerOrHuman(CHOption) ,
                           (CHOption == 1 -> (easyOrHard(EHOptiont) , EHOption is EHOptiont + 1 , playerNames(CHOption, EHOption)) ;
                            playerNames(CHOption, _)) , sleep(1) , playGame) ;
         load_game).

:- initialization(startGame).
