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

printVerticalLines(-1):-!.
printVerticalLines(D):-write('│  '), C is D-1, printVerticalLines(C).
printBottomLine(0):-!.
printBottomLine(D,C):- (C == D -> write('└') ; (write('───'), C1 is C-1, printBottomLine(D,C1))).

                           
display([],_,_,_).                
                                                                                                                   
display([Elem|Rest], Dim, C , L):- (L < Dim -> 
        ((C == 0, L == 0 -> (write('┌─'), C1 is C+1, display([Elem|Rest],Dim, C1, L));
          ((C == 0 , L \= 0 -> (write('├─'), C1 is C+1, display([Elem|Rest],Dim, C1, L));
            ((C < Dim -> (write('─┬─'), C1 is C + 1, display([Elem|Rest],Dim, C1, L));
              (write('─┐'), nl, printVerticalLines(Dim),
              ((C == Dim -> L1 is L + 1, nl, display([Elem|Rest],Dim, 0, L1))   ))))))));
                                    printBottomLine(Dim, Dim)  ).
           

print_(Elem):-(Elem == e -> write('┌──'));
              (Elem == w -> write('──┐'));
              (Elem == n -> write('└──'));
              (Elem == s -> write('┌──'));
              (Elem == cross -> write('─┬─')).

%┬
%┴

opposite(Cardinal, Opos):-(Cardinal == e -> Opos=w);
                          (Cardinal == w -> Opos=e);
                          (Cardinal == n -> Opos=s);
                          (Cardinal == s -> Opos=n).
                    
dictionary(Attr, Code):-(Attr == + -> Code == 1);
                        (Attr == - -> Code == -1);
                        (Attr == x -> Code == 0).                     
                                             
checkAdjacentPiece(MyCardinal, List):- opposite(MyCardinal,Opos), nth1(1,List,Opos). 