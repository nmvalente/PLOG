/* -*- Mode:Prolog; coding:iso-8859-1; -*- */

:- consult(generator).

generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Time, Backtracks) :-
        fd_statistics(backtracks, BackStart) ,
        statistics(walltime, [Start,_]) ,
        getRandomPuzzle(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus) ,
        statistics(walltime, [End,_]) ,
        fd_statistics(backtracks, BackEnd) ,
        Time is End - Start ,
        Backtracks is BackEnd - BackStart.

solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, Time, Backtracks, Options) :-
        fd_statistics(backtracks, BackStart) ,
        statistics(walltime, [Start,_]) ,
        solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, Options) ,
        statistics(walltime, [End,_]) ,
        fd_statistics(backtracks, BackEnd) ,
        Time is End - Start ,
        Backtracks is BackEnd - BackStart.
        
run(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, Options) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen, BackGen) ,
        solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Result, TimeSol, BackSol, Options) , 
        format('Puzzle of size ~d generated in ~3d seconds (~d backtracks) and solved in ~3d seconds (~d backtracks).~n', [PuzzleSize, TimeGen, BackGen, TimeSol, BackSol]).

run(PuzzleSize, Options) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen, BackGen) ,
        solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, TimeSol, BackSol, Options) , 
        format('Puzzle of size ~d generated in ~3d seconds (~d backtracks) and solved in ~3d seconds (~d backtracks).~n', [PuzzleSize, TimeGen, BackGen, TimeSol, BackSol]).

runAll(PuzzleSize, Options) :-
        generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, TimeGen, _) ,
        statistics(walltime, [Start,_]) ,
        findall(Result, solvePuzzle(Result, Puzzle, PuzzleSize, Rminus, Rplus, Cminus, Cplus, Options), List) ,
        statistics(walltime, [End,_]) ,
        TimeSol is End - Start ,
        length(List, N) ,
        TimeAvg is TimeSol / N ,
        format('Puzzle of size ~d generated in ~3d seconds, ~d solutions found in ~3d seconds, ', [PuzzleSize, TimeGen, N, TimeSol]) ,
        format('average time per solution is ~3d seconds.~n', [TimeAvg]).

runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, MaxTimeGen, MaxTimeSol, MinTimeGen, MinTimeSol, MaxGen, MaxSol, MinGen, MinSol, Counter, TimeGen, TimeSol, TotalBackGen, TotalBackSol, BackGen, BackSol, Options) :-
        Counter > PuzzleNumber -> 
        (TotalTimeGen = TimeGen ,
         TotalTimeSol = TimeSol , 
         MaxTimeGen = MaxGen , 
         MaxTimeSol = MaxSol , 
         MinTimeGen = MinGen , 
         MinTimeSol = MinSol,
         TotalBackGen = BackGen ,
         TotalBackSol = BackSol) ;
        (generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, Time1, Back1) ,
         TimeGen1 is TimeGen + Time1 ,
         BackGen1 is BackGen + Back1 ,
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time2, Back2, Options) , 
         TimeSol1 is TimeSol + Time2 ,
         BackSol1 is BackSol + Back2 ,
         Counter1 is Counter + 1 ,
         (Time1 > MaxGen ->
          MaxGen1 = Time1 ;
          MaxGen1 = MaxGen) ,
         (Time1 < MinGen ->
          MinGen1 = Time1 ;
          MinGen1 = MinGen) ,
         (Time2 > MaxSol ->
          MaxSol1 = Time2 ;
          MaxSol1 = MaxSol) ,
         (Time2 < MinSol ->
          MinSol1 = Time2 ;
          MinSol1 = MinSol) ,
         runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, MaxTimeGen, MaxTimeSol, MinTimeGen, MinTimeSol, MaxGen1, MaxSol1, MinGen1, MinSol1, Counter1, TimeGen1, TimeSol1, TotalBackGen, TotalBackSol, BackGen1, BackSol1, Options)).

run(PuzzleSize, PuzzleNumber, Options, MinGen, AvgGen, MaxGen, BackGen, MinSol, AvgSol, MaxSol, BackSol) :-
        runAvg(PuzzleSize, PuzzleNumber, TotalTimeGen, TotalTimeSol, MaxGen, MaxSol, MinGen, MinSol, 0, 0, 3600000000, 3600000000, 1, 0, 0, TotalBackGen, TotalBackSol, 0, 0, Options) ,
        AvgGen is TotalTimeGen / PuzzleNumber ,
        AvgSol is TotalTimeSol / PuzzleNumber ,
        BackGen is TotalBackGen / PuzzleNumber ,
        BackSol is TotalBackSol / PuzzleNumber.

solveAvg(PuzzleSize, PuzzleNumber, Counter ,
         TotalTime_none, MaxTime_none, MinTime_none, Max_none, Min_none, Time_none, TotalBack_none, Back_none,
         TotalTime_ff, MaxTime_ff, MinTime_ff, Max_ff, Min_ff, Time_ff, TotalBack_ff, Back_ff,
         TotalTime_median, MaxTime_median, MinTime_median, Max_median, Min_median, Time_median, TotalBack_median, Back_median,
         TotalTime_all, MaxTime_all, MinTime_all, Max_all, Min_all, Time_all, TotalBack_all, Back_all) :-
        Counter > PuzzleNumber -> 
        (TotalTime_none = Time_none , 
         MaxTime_none = Max_none , 
         MinTime_none = Min_none,
         TotalBack_none = Back_none,
         TotalTime_ff = Time_ff , 
         MaxTime_ff = Max_ff , 
         MinTime_ff = Min_ff,
         TotalBack_ff = Back_ff,
         TotalTime_median = Time_median , 
         MaxTime_median = Max_median , 
         MinTime_median = Min_median,
         TotalBack_median = Back_median,
         TotalTime_all = Time_all , 
         MaxTime_all = Max_all , 
         MinTime_all = Min_all,
         TotalBack_all = Back_all) ;
        (generate(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, _) ,
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_none, Back0_none, []) , 
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_ff, Back0_ff, [ff]) , 
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_median, Back0_median, [median]) , 
         solve(PuzzleSize, Puzzle, Rminus, Rplus, Cminus, Cplus, _, Time0_all, Back0_all, [ff, median]) , 
         Time1_none is Time_none + Time0_none ,
         Back1_none is Back_none + Back0_none ,
         Time1_ff is Time_ff + Time0_ff ,
         Back1_ff is Back_ff + Back0_ff ,
         Time1_median is Time_median + Time0_median ,
         Back1_median is Back_median + Back0_median ,
         Time1_all is Time_all + Time0_all ,
         Back1_all is Back_all + Back0_all ,
         Counter1 is Counter + 1 ,
         (Time0_none > Max_none ->
          Max1_none = Time0_none;
          Max1_none = Max_none) ,
         (Time0_none < Min_none ->
          Min1_none = Time0_none ;
          Min1_none = Min_none) ,
         (Time0_ff > Max_ff ->
          Max1_ff = Time0_ff;
          Max1_ff = Max_ff) ,
         (Time0_ff < Min_ff ->
          Min1_ff = Time0_ff ;
          Min1_ff = Min_ff) ,
         (Time0_median > Max_median ->
          Max1_median = Time0_median;
          Max1_median = Max_median) ,
         (Time0_median < Min_median ->
          Min1_median = Time0_median ;
          Min1_median = Min_median) ,
         (Time0_all > Max_all ->
          Max1_all = Time0_all;
          Max1_all = Max_all) ,
         (Time0_all < Min_all ->
          Min1_all = Time0_all ;
          Min1_all = Min_all) ,
         solveAvg(PuzzleSize, PuzzleNumber, Counter1 ,
         TotalTime_none, MaxTime_none, MinTime_none, Max1_none, Min1_none, Time1_none, TotalBack_none, Back1_none,
         TotalTime_ff, MaxTime_ff, MinTime_ff, Max1_ff, Min1_ff, Time1_ff, TotalBack_ff, Back1_ff,
         TotalTime_median, MaxTime_median, MinTime_median, Max1_median, Min1_median, Time1_median, TotalBack_median, Back1_median,
         TotalTime_all, MaxTime_all, MinTime_all, Max1_all, Min1_all, Time1_all, TotalBack_all, Back1_all)).

run(PuzzleSize, PuzzleNumber, 
    Avg_none, Max_none, Min_none, Back_none,
    Avg_ff, Max_ff, Min_ff, Back_ff,
    Avg_median, Max_median, Min_median, Back_median,
    Avg_all, Max_all, Min_all, Back_all) :-
        solveAvg(PuzzleSize, PuzzleNumber, 0 ,
                 TotalTime_none, Max_none, Min_none, 0, 3600000000, 0, TotalBack_none, 0,
                 TotalTime_ff, Max_ff, Min_ff, 0, 3600000000, 0, TotalBack_ff, 0,
                 TotalTime_median, Max_median, Min_median, 0, 3600000000, 0, TotalBack_median, 0,
                 TotalTime_all, Max_all, Min_all, 0, 3600000000, 0, TotalBack_all, 0) ,
        Avg_none is TotalTime_none / PuzzleNumber ,
        Avg_ff is TotalTime_ff / PuzzleNumber ,
        Avg_median is TotalTime_median / PuzzleNumber ,
        Avg_all is TotalTime_all / PuzzleNumber ,
        Back_none is TotalBack_none / PuzzleNumber ,
        Back_ff is TotalBack_ff / PuzzleNumber ,
        Back_median is TotalBack_median / PuzzleNumber ,
        Back_all is TotalBack_all / PuzzleNumber.
