                      
getSpiral([Element|Spiral], [NextDirection|Directions], Size, CurrentSize, Direction, Index, Line, Column) :-
        CurrentSize == 0 -> 
        (Element = -1 , Spiral = [] , NextDirection = 0, Directions = []) ;
        (Index > CurrentSize ->
         (Direction == 4 ->
          getSpiral([Element|Spiral], [NextDirection|Directions], Size, CurrentSize, 1, 1, Line, Column) ;
          (Direction1 is Direction + 1 ,
           (member(Direction1, [2, 4]) -> 
            (CurrentSize1 is CurrentSize - 1 ,
             getSpiral([Element|Spiral], [NextDirection|Directions], Size, CurrentSize1, Direction1, 1, Line, Column)) ;
            getSpiral([Element|Spiral], [NextDirection|Directions], Size, CurrentSize, Direction1, 1, Line, Column)))) ;
         (Index1 is Index + 1 ,
          (Direction == 1 ->
           (Column1 is Column + 1 ,
            Element is (Line - 1) * Size + Column1 ,
            NextDirection is Direction ,
            getSpiral(Spiral, Directions, Size, CurrentSize, Direction, Index1, Line, Column1)) ;
           (Direction == 2 ->
            (Line1 is Line + 1 ,
             Element is (Line1 - 1) * Size + Column ,
             NextDirection is Direction ,
             getSpiral(Spiral, Directions, Size, CurrentSize, Direction, Index1, Line1, Column)) ;
            (Direction == 3 ->
             (Column1 is Column - 1 ,
              Element is (Line - 1) * Size + Column1 ,
              NextDirection is Direction ,
              getSpiral(Spiral, Directions, Size, CurrentSize, Direction, Index1, Line, Column1)) ;
             (Line1 is Line - 1 ,
              Element is (Line1 - 1) * Size + Column ,
              NextDirection is Direction ,
              getSpiral(Spiral, Directions, Size, CurrentSize, Direction, Index1, Line1, Column))))))).
getSpiral(Spiral, Directions, Size) :-
        getSpiral(Spiral1, Directions1, Size, Size, 1, 1, 1, 0) ,
        append(Spiral, [_], Spiral1) ,
        append(Directions, [_], Directions1).

