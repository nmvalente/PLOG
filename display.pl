/* -*- Mode:Prolog; coding:utf-8; -*- */

:- use_module(library(lists)).

e1Puzzle([e,w,e,w,s,e,w,s,n,e,w,n,e,w,e,w]).
copye1Puzzle([e,w,e,w,s,e,w,s,n,e,w,n,e,w,e,w]).


e1Numbers([1,-1,0,0,1,1,-1,0,-1,1,-1,0,1,-1,0,0]).
lMinus([1,1,2,1]).
cMinus([1,2,1,0]).
lPlus([1,2,1,1]).
cPlus([3,2,1,0]). 

getPuzzle(Puzzle):-e1Puzzle(Puzzle).
getLMinus(LMinus):-lMinus(LMinus).
getCMinus(CMinus):-cMinus(CMinus).
getLPlus(LPlus):-lPlus(LPlus).
getCPlus(CPlus):-cPlus(CPlus).

puzzleInitialization(Puzzle, LMinus, CMinus, LPlus, CPlus):-getPuzzle(Puzzle),
                                                            getLMinus(LMinus), 
                                                            getLPlus(LPlus),
                                                            getCMinus(CMinus), 
                                                            getCPlus(CPlus).

%%printMagnets(Puzzle, LMinus, CMinus, LPlus, CPlus, _):-puzzleInitialization(Puzzle, LMinus, CMinus, LPlus, CPlus).

printMagnets(Puzzle, _, _, _, _, Dim):-puzzleInitialization(Puzzle, _, _, _, _), display(Puzzle, Dim, 0 , 0).

printVerticalLines(0):-!.
printVerticalLines(D):-write('│'), C is D-1, printVerticalLines(C).

display([],_,_,_).                                                                                                                                   
display([Elem|Rest], Dim, C , L):-((C < Dim , C > 0)-> write('─┬─'), C1 is C + 1, display([Elem|Rest],Dim, C1, L));
                                  (C == 0 -> write('┌─')). 
                 
        
print_(Elem):-(Elem == e -> write('┌──'));
              (Elem == w -> write('──┐'));
              (Elem == n -> write('└──'));
              (Elem == s -> write('┌──'));
              (Elem == cross -> write('─┬─')).

%(C == Dim-1 -> write('─┐')),
%┬
%┴
/*
  ((C == 0) -> (write('┌─'), C1 is C+1, display(Rest, Dim, C1, 0)));               % inicio de linha
        ((L < Dim) -> (((C < Dim-1) -> (print_(cross), C2 is C+1, display(Rest, Dim, C2, L)));
                       ((C == Dim-1) -> (print_(cross), write('──┐'), L1 is L+1, display(Rest, Dim, 0, L1)));
                       ((nl, L1 is L+1, display([Elem|Rest], Dim, 0, L1))))).  
 */


opposite(Cardinal, Opos):-(Cardinal == e -> Opos=w);
                          (Cardinal == w -> Opos=e);
                          (Cardinal == n -> Opos=s);
                          (Cardinal == s -> Opos=n).
                    
dictionary(Attr, Code):-(Attr == + -> Code == 1);
                        (Attr == - -> Code == -1);
                        (Attr == x -> Code == 0).                     
                                             
checkAdjacentPiece(MyCardinal, List):- opposite(MyCardinal,Opos), nth1(1,List,Opos). 