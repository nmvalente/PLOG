/* -*- Mode:Prolog; coding:utf-8; -*- */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%  CONFIGURACOES INICIAIS DO JOGO  %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
halfPiece(N,L,C):-N,L,C.
display_halfPiece(halfPiece(N,L,C)):-write(N),write(L),write(C).

initial_board( 
 [
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___']
        ]).

 
playerChange(1,2).
playerChange(2,1).

playerTurn(1).

set1([piece(0, 1), piece(0, 3), piece(0, 4), piece(0, 6), piece(1, 1), piece(1, 2), piece(1, 4), piece(1, 5), piece(1, 7), piece(2, 2), piece(2, 3), piece(2, 6), piece(3, 3), piece(3, 7), piece(4, 5), piece(5, 6), piece(6, 7), piece(7, 7)]).
set2([piece(0, 0), piece(0, 2), piece(0, 5), piece(0, 7), piece(1, 3), piece(1, 6), piece(2, 4), piece(2, 5), piece(2, 7), piece(3, 4), piece(3, 5), piece(3, 6), piece(4, 4), piece(4, 6), piece(4, 7), piece(5, 5), piece(5, 7), piece(6, 6)]).

state(set1, set2, playerTurn, board).
initial_state(set1, set2, startPlayer, initial_board).

initial_screen:- 
        write('     DOMINUP'), nl , write('PLOG FEUP 2015-16'), nl, write('Ângela Cardoso'), nl, write('Nuno Valente'), nl, nl.



start_game:-
                                %abolish(jogador/2), abolish(estado/4),
        initial_screen,
                                %game_type,
        initial_state(set1, set2, playerTurn, board).
                                %play().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%  VISUALIZACAO DO TABULEIRO  %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%initial_board(X),display_game(X). for tests


display_game([X|Y]) :- nl, write('    A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   Y   X'), nl,
                       write('    ______________________________________________________________________________________________'), nl,
                       rows(1, [X|Y]), nl,
                       %write('   _______________________________________________________________________________________________'), nl,
                       write('    A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   Y   X'), nl, nl,!.

rows(N, [X|Y]):-((N<10, write(0), write(N), write('|')) ; (write(N), write('|'))), (analyse(X), N1 is N+1, nl, rows(N1, Y)).
rows(_,[]).

analyse([]):-!.
analyse([X|Z]):-write(X),  write('|'), analyse(Z).

           
/*
traduz(','):-write(' | ').

*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  POSICAO INTERMEDIA DE JOGO  %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

intermediate_board( 
        [
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','71s','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','71n','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___']
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  FINAL DO JOGO  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


final_board( 
        [
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___'],
   ['___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___','___']
        ]).



%Verificacao do vencedor e de chegada ao final do jogo

game_over(Winner, state(Set1, Set2, Pturn, _)):-((Set1 == [], Pturn is 2, Winner is 1) ; (Set2 == [], Pturn is 1, Winner is 2)), message(Winner).

message(Winner):-write('Player '), write(Winner), write(' won').
