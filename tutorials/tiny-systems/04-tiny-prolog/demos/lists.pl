n1to3([1,2,3]).
n1to9([1,2,3,4,5,6,7,8,9]).

% WORKING WIHT LISTS
% head/tail patterns with n1to3
% append(X,Y,Z) using recursion
% sum(X, N) to sum list elements (using 'is')
% reverse(X, Acc, Y) using accumulator trick
append([], X, X).
append([X|Xs], Y, [X|Z]) :- append(Xs, Y, Z).

% GENERATING MAGIC SQUARES
% using n1to9 of Ns and permutations of Ns
magic([A1,A2,A3,B1,B2,B3,C1,C2,C3]) :- 
  n1to9(Ns),
  permutation(Ns, [A1,A2,A3,B1,B2,B3,C1,C2,C3]),
  15 is A1 + B2 + C3,
  15 is A3 + B2 + C1,
  15 is A1 + A2 + A3,
  15 is B1 + B2 + B3,
  15 is C1 + C2 + C3,
  15 is A1 + B1 + C1,
  15 is A2 + B2 + C2,
  15 is A3 + B3 + C3.
  
