-module(week1).
-export([perimeter/1, area/1, enclose/1, bits/1, testp/0, testa/0,  teste/0, testb/0]).


%--------------------------------
%---------------SHAPES-----------
% Define a function perimeter/1 which takes a shape and returns the perimeter of the shape.
% It takes a list of points/tuple with head starting points and the last one exactly same as start points to close the shape, for example a triangle would be [{0, 0}, {1,0}, {0, 1}, {0, 0}], with {0, 0} be the starting and end points
perimeter([Point | T = [NextPoint | _]]) ->
	distance(Point, NextPoint) + perimeter(T);
perimeter([_Point]) ->
	0.

% calculate the distance of two points. points is represented in the form of tuple
distance({X1, Y1}, {X2, Y2}) ->
	math:sqrt((X2 - X1) * (X2 - X1) + (Y2 - Y1) * (Y2 - Y1)).

% calculate the area of a given triangle
% Heron's formula is used here. https://en.wikipedia.org/wiki/Heron's_formula
% It takes a list of points/tuple with head starting points and the last one exactly same as start points to close the shape, for example a triangle would be [{0, 0}, {1,0}, {0, 1}, {0, 0}], with {0, 0} be the starting and end points
area(Points = [Point1, Point2, Point3, Point1]) ->
	P = perimeter(Points)/2,
	A = distance(Point1, Point2),
	B = distance(Point2, Point3),
	C = distance(Point3, Point1),
	math:sqrt(P*(P-A)*(P-B)*(P-C)).

% Define a function enclose/1 that takes a shape an returns the smallest enclosing rectangle of the shape.
% Return value is a tuple contains the three points of triangle and the area of it
% 0 area is excluded from the result since it is not a triangle shape but a line
% Example:
%        > week1:enclose([{0, 0}, {1,0}, {1, 1}, {0, 1}, {-1, 1}, {0, 0}]).
%        {[{1,0},{0,1},{-1,1}],0.4999999999999996}
enclose(Points) ->
	[_ | NPoints] = Points, % get rid of repetitive starting point, since the endo point is the same one
	ResultList = lists:foldl(fun(Ps, Acc) -> [{Ps, area(Ps ++ [hd(Ps)])} | Acc ] end,
		    [],
		    combos(3, NPoints)
		   ),
	FilteredList = lists:filter(fun({_, A}) -> A /= 0 end, ResultList),
	Result = lists:sort(fun({_, A1}, {_, A2}) -> A1 < A2 end,
		FilteredList	
	 ),
	hd(Result).

% got idea about calculating combo from panduwana.wordpress.com
% https://panduwana.wordpress.com/2010/04/21/combination-in-erlang/
combos(1, L) -> [[X] || X <-L];
combos(K, L) when K == length(L) -> [L];
combos(K, [H|T]) ->
	[[H | Subcombos] || Subcombos <- combos(K-1, T)]
	++(combos(K, T)).


%------------------------------------------
%---------------Summing the Bits-----------
% Summing the bits
bits(B) ->
	R = hd(io_lib:format("~.2B", [B])),
	recur_bits(R).
% comment the above line and uncomment the following two lines will use comprehension to solve the problem
%	L = [X || X <- R, X /= 48 ],
%	length(L).



% a recursive solutions to bits
recur_bits([H | T]) ->
	(H - 48) + recur_bits(T);
recur_bits([]) ->
	0.


%--------------------------------------------------
%------- HERE STARTS the test data-----------------
% choose a reprensentation of triangles and use area/1 and perimeter/1 to handle
% test data to calculate prerimeter
testp() ->
	perimeter([{0, 0}, {1,0}, {0, 1}, {0, 0}]).

% test data to calculate a triangle area
testa() ->
	area([{0, 0}, {1,0}, {0, 1}, {0, 0}]).

% test data to calculate the enclose/1
teste() -> 
	enclose([{0, 0}, {1,0}, {1, 1}, {0, 1}, {-1, 1}, {0, 0}]).

% given a decimal number calculate the number of ones in the binary
testb() ->
	bits(7).
