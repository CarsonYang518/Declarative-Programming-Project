% ---------------------------------------------------------------------
% Author: Kaixun Yang <kaixuny@student.unimelb.edu.au>
% Purpose: Solve maths puzzles in Prolog
% 
% Maths puzzle is a square grid of squares, each to be filled in with a 
% single digit 1–9 satisfying three constraints:
% 1.each row and each column contains no repeated digits (the row and 
% column headings are not considered to be part of the row or column);
% 2.all squares on the diagonal line from upper left to lower right 
% contain the same value;
% 3.the heading of reach row and column holds either the sum or the 
% product of all the digits in that row or column.
%
% Date: 19/04/2021
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% Load the library clpfd module containing transpose/2, #=/2, 
% all_distinct/1, ins/2, labeling/2

:- ensure_loaded(library(clpfd)).
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% Predicate puzzle_solution(Puzzle) holds when Puzzle is the 
% representation of a solved maths puzzle.
% 
% Input: 
% A list of lists (all the header squares of the puzzle (plus the 
% ignored corner square) are bound to integers. Some of the other 
% squares in the puzzle may also be bound to integers, but the others 
% will be unbound.)
%
% Operations:
% 1. Use maplist/2 and same_length/2 to ensure that the length
% of Puzzle is the same as the length of each row. (Square Matrix).
% 2. Use transpose/2 to get the transposed puzzle, which help us
% check the columns by checking the rows of the transposed puzzle.
% 3. Use domain_constraint_rows/1 to ensure that each square grid is 
% filled in with a single digit 1–9.
% 4. Use no_repeated_rows/1 to ensure that each row and each column 
% contains no repeated digits.
% 5. Use same_diagonal/1 to ensure that all squares on the diagonal 
% line from upper left to lower right contain the same value.
% 6. Use head_constraint_rows/1 to ensure that the heading of reach 
% row and column holds either the sum or the product of all the 
% digits in that row or column.
% 7. Use ground_rows/1 to ensure that all squares in each row have 
% already been filled.

puzzle_solution(Puzzle) :- 
    maplist(same_length(Puzzle), Puzzle),
    transpose(Puzzle, T_Puzzle),
    domain_constraint_rows(Puzzle),
    no_repeated_rows(Puzzle),
    no_repeated_rows(T_Puzzle),
    same_diagonal(Puzzle),
    head_constraint_rows(Puzzle),
    head_constraint_rows(T_Puzzle),
    ground_rows(Puzzle).
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% Predicate domain_constraint_rows/1 holds when each row(except 
% for heading row) satisfies domain_constraint/1.
% 
% Input: A list of lists
%
% Operations: Use maplist/2 and domain_constraint/1 to ensure that 
% each row (except for heading row) satisfies domain_constraint.

domain_constraint_rows([_|Rows]) :-
    maplist(domain_constraint, Rows).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% Predicate domain_constraint/1 holds when a list satisfies that 
% all elements in the list (except for the heading element) are greater 
% or equal to 1 and less than or equal to 9.
%
% Input: A list
%
% Operations: Use ins/2 to ensure that all elements in the list (except 
% for the heading element) are greater or equal to 1 and less than or 
% equal to 9.

domain_constraint([_|Row]) :- 
    Row ins 1..9.
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% Predicate no_repeated_rows/1 holds when each row(except for 
% heading row) satisfies no_repeated/1.
%
% Input: A list of lists
%
% Operations: Use maplist/2 and no_repeated/1 to ensure that each row 
% (except for heading row) satisfies no_repeated.

no_repeated_rows([_|Rows]) :-
    maplist(no_repeated, Rows).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% Predicate no_repeated/1 holds when a list satisfies that all elements 
% in the list (except for the heading element) are different.
%
% Input: A list
%
% Operations: Use all_distinct/1 to ensure that all elements in the list 
% (except for the heading element) are different.

no_repeated([_|Row]) :- 
    all_distinct(Row).
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% Predicate same_diagonal/1 holds when all the elements in the diagonal 
% list of the matrix (except the first heading row) are identical.
%
% Input: A list of lists
%
% Operations: Use diagonal/3 to get the diagonal elements in the matrix
% (except the first heading row), use all_same/1 to ensure that the all 
% elements of the diagonal list are identical.

same_diagonal([_|Rows]) :- 
    diagonal(Rows,1,D), all_same(D).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% Predicate diagonal/3 holds when the matrix is empty and D is a empty list.
% Predicate diagonal/3 holds when the D is the diagonal list of matrix.
%
% Input: Two list, a integer
%
% Operations: Use nth0/3 to get the I_th element(start with 0) in the row, 
% ensure that the element is in the list D, use recursion to get the (I+2)_th 
% element in next row, ensure that it's also in the list D, Do the recursions 
% until the base case.

diagonal([], _ ,[]).
diagonal([Row|Rows],I,D) :- 
    (nth0(I, Row, E)
    -> D = [E|Ds],
       I1 #= I + 1,
       diagonal(Rows, I1, Ds)
    ;  D = []).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% Predicate all_same()/1 holds when the list is empty.
% Predicate all_same()/1 holds when the list only contains one element.
% Predicate all_same()/1 holds when the all elements in list is identical.
%
% Input: A list
%
% Operations: Compare the first two elements, then use recursion to check
% whether the sencond and third elements are same until the base.

all_same([]).
all_same(List) :- length(List, 1).
all_same([E1, E2|Es]) :- E1 #= E2, all_same([E2|Es]).
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% Predicate sum/2 holds when the list is empty and the integer is 0.
% Predicate sum/2 holds when the integer is the sum of each elements in 
% list.
%
% Input: A list, a integer
%
% Operations: Use recursion to get the sum of each elements in list 
% until the base case.

sum([], 0).
sum([X|Xs], S):-
    sum(Xs, S1),
    S #= S1 + X.
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% Predicate product/2 holds when the list is empty and the integer is 1.
% Predicate product/2 holds when the integer is the product of each 
% elements in list.
%
% Input: A list, a integer
%
% Operations: Use recursion to get the product of each elements in list 
% until the base case.

product([], 1).
product([X|Xs], P) :-
    product(Xs, P1),
    P #= P1 * X.
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% Predicate head_constraint_rows/1 holds when each row(except for 
% heading row) satisfies head_constraint/1.
% 
% Input: A list of lists
%
% Operations: Use maplist/2 and head_constraint/1 to ensure that each 
% row (except for heading row) satisfies head_constraint.

head_constraint_rows([_|Rows]) :-
    maplist(head_constraint, Rows).
% ---------------------------------------------------------------------
% ---------------------------------------------------------------------
% Predicate head_constraint/1 holds when the first element is the sum or 
% product of the rest elements in list.
% 
% Input: A list
%
% Operations: Use sum/2 or product/2 to get the sum or product of all 
% elements in list(except for the first heading element), use #=/2 to
% ensure that the heading element is the sum or product of the rest elements.

head_constraint([X|Xs]) :- 
    (sum(Xs, S), X #= S);
    (product(Xs, P), X #= P).
% ---------------------------------------------------------------------

% ---------------------------------------------------------------------
% Predicate ground_rows/1 holds when all variables in each row is ground.
% 
% Input: A list of lists
%
% Operations: Use maplist/2 and labeling/2 ensure that all variables in 
% each row systematically try out values in the finite domain until all 
% of them are ground.

ground_rows([_|Rows]) :- 
    maplist(labeling([]), Rows).
% ---------------------------------------------------------------------