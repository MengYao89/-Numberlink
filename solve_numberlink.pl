/* ............................................................. */
/* ............................................................. */
/* ............................................................. */
%% Main
main(InputFile,OutputFile):-
    phrase_from_file(maze(Size,Number,List),InputFile),
    solve(Size,List,Maze),
    maze2paths(List,Maze,Paths),
    print_result(OutputFile,Size,Number,Paths). 
    
/* ............................................................. */
%% Input
maze(Size,Number,List) --> first_at(Size,Number),
                           middle_ats(Init),
                           end_at(Last),
                           {append(Init,[Last],List)}.
                           
first_at(X,Y) --> "(",spaces,
                     suffix(CX),spaces,{code2digit(CX,X)},
                     ",",spaces,
                     suffix(CY),spaces,{code2digit(CY,Y)},
                     ",",spaces,"\n".

middle_ats([X|T]) --> middle_at(X),middle_ats(T).
middle_ats([])    --> [].
                     
middle_at((N,L)) --> "(",spaces,
                   suffix(CN),spaces,{code2digit(CN,N)},
                   ":",spaces,
                   points(L),spaces,
                   ")",spaces,
                   ",",spaces,"\n".

end_at((N,L))   --> "(",spaces,
                   suffix(CN),spaces,{code2digit(CN,N)},
                   ":",spaces,
                   points(L),spaces,
                   ")",spaces,
                   ")",(spaces;"/n").
                   
points([H,T]) --> point(X1,Y1),spaces,
                  {H = (X1,Y1)},
                  ",",spaces,
                  point(X2,Y2),spaces,
                  {T = (X2,Y2)}.
points([])    --> [].

point(X,Y) --> "(",spaces,
             suffix(CX),spaces,{code2digit(CX,X)},
             ",",spaces,
             suffix(CY),spaces,{code2digit(CY,Y)},
             ")".
             
suffix([H|T]) -->
      [H], 
      {
          code_type(H, digit)
      },
      suffix(T).
suffix([]) --> []. 

spaces --> [].
spaces --> " ", spaces.

code2digit([],0).
code2digit([X|T],Digit):- Num is X - 48,
                    length([X|T],Len),
                    code2digit(T,SD),
                    Digit is Num * 10 ** (Len - 1) + SD.
/* ............................................................. */
%% Solve
solve(Size,List,Maze) :-
    get_maze(Size,List,Maze),
    all_connected(List,Maze).

get_maze_board(Size,Maze) :- 
               length(Maze,Size),
               maplist(map_length(Size),Maze).

get_maze(Size,List,Maze) :- 
           get_maze_board(Size,Maze),
           get_maze(List,Maze).    

get_maze([],_).           
get_maze([(Number,[(X1,Y1),(X2,Y2)])|T],Maze):-
            nth1(Y1, Maze, R1),
            nth1(X1, R1, Number),
            nth1(Y2, Maze, R2),
            nth1(X2, R2, Number),
            get_maze(T,Maze).

connected(Point1, Point2, Maze, Visited) :-
    adj(Point1, Point2),
    maplist(dif(Point2), Visited),
    at(Point1, Number, Maze),
    at(Point2, Number, Maze).
connected(Point1, Point2, Maze, Visited) :-
    adj(Point1, Point3),
    maplist(dif(Point3), Visited),
    at(Point1, Number, Maze),
    at(Point3, Number, Maze),
    connected(Point3, Point2, Maze, [Point3|Visited]).

all_connected([],_).    
all_connected([(_,[Point1,Point2])|T],Maze):-
    connected(Point1, Point2, Maze, [Point1]),
    all_connected(T,Maze).

/* ............................................................. */
%% Output
print_result(File,Size,Number,Paths) :-
  open(File,write,Stream),
  format_firstline(Stream,Size,Number),
  format_middleline(Stream,Paths),
  close(Stream).

format_firstline(Stream,Size,Number):-
  format(Stream, "(~d, ~d, ~n",[Size,Number]).

format_middleline(Stream,[(Number,Path)]):-
    format(Stream, "(~d: ",[Number]),
    format_path(Stream,Path),
    format(Stream, ")",[]).  
  
format_middleline(Stream,[(Number,Path)|Paths]):-  
    format_pathi(Stream,(Number,Path)),
    format_middleline(Stream,Paths).
  
format_pathi(Stream,(Number,Path)):-
    format(Stream, "(~d: ",[Number]),
    format_path(Stream,Path),
    format(Stream, ",~n",[]).
  
format_path(Stream,[(Y,X)]):-
  format(Stream, "(~d,~d))",[Y,X]).
format_path(Stream,[(Y,X)|Path]) :-
  format(Stream, "(~d,~d), ",[Y,X]),
  format_path(Stream,Path).


maze2paths([],_,[]).
maze2paths([H|T],Maze,Paths):-
           maze2pathi(H,Maze,Path),
           maze2paths(T,Maze,TailPaths),
           Paths = [Path|TailPaths].

maze2pathi((Number,Points),Maze,(Number,Path)):-
          maze2path((Number,Points),Maze,Path).
          
maze2path((Number,[Point1,Point2]),Maze,Path):-
          maze2path((Number,[Point1,Point2]),Maze,[Point1],Path),!. 
          
maze2path((Number,[_,Point2]),Maze,Acc,Path):-
            append(_,[Point],Acc),
            adj(Point,Point2),
            maplist(dif(Point2), Acc),
            at(Point2,Number,Maze),
            append(Acc,[Point2],Path).
            
maze2path((Number,Points),Maze,Acc,Path):-
            append(_,[Point],Acc),
            adj(Point,NewPoint),
            maplist(dif(NewPoint), Acc),
            at(NewPoint,Number,Maze),
            append(Acc,[NewPoint],NewAcc),
            maze2path((Number,Points),Maze,NewAcc,Path).            
/* ............................................................. */
%% helper
map_length(L,X):-length(X,L).

adj((X,Y1), (X,Y2)) :- Y2 is Y1+1.
adj((X,Y1), (X,Y2)) :- Y2 is Y1-1.
adj((X1,Y), (X2,Y)) :- X2 is X1+1.
adj((X1,Y), (X2,Y)) :- X2 is X1-1.

at((X,Y), Number, Maze) :-
    nth1(Y, Maze, Row),
    nth1(X, Row, Number).