0.5::pick(N, a) ; 0.5::pick(N,b).

% a palindrome of length N spans positions 1 to N
palindrome(N) :-
      palindrome(1,N).

% base case for even length: left and right crossed
palindrome(A,B) :-
      A > B.
% base case for uneven length: arbitrary middle character
palindrome(N,N) :-
      pick(N,X).
% recursive case: add same character at both ends and move positions towards the middle
palindrome(A,B) :-
      A < B,
      pick(A,X),
      pick(B,X),
      AA is A+1,
      BB is B-1,
      palindrome(AA,BB).

bb(N) :-
      Max is N-1,
      between(1,Max,I),
      pick(I,b),
      II is I+1,
      pick(II,b).

len(30).
query(palindrome(X)) :- len(X).
%query(bb(X)) :- len(X).

% m-067:ProbLog sad$ time problog stringsFast.pl
% palindrome(10):	0.03125
% real	0m0.340s
% user	0m0.243s
% sys	0m0.092s
%
% m-067:ProbLog sad$ time problog stringsFast.pl
% palindrome(20):	0.0009765625
% 
% real	0m1.401s
% user	0m1.196s
% sys	0m0.153s
%
% m-067:ProbLog sad$ time problog stringsFast.pl
% palindrome(25):	0.00024414062
% 
% real	0m2.357s
% user	0m2.148s
% sys	0m0.186s
%
% m-067:ProbLog sad$ time problog stringsFast.pl
% palindrome(30):	3.0517578e-05
% 
% real	0m13.770s
% user	0m13.180s
% sys	0m0.427s

% m-067:ProbLog sad$ time problog stringsFast.pl
% palindrome(35):	7.6293945e-06
% 
% real	0m46.038s
% user	0m41.213s
% sys	0m1.382s
