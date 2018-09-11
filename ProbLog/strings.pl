0.5::pick(N, a) ; 0.5::pick(N,b).

random_string(0,[]).
random_string(N,[X|L]) :-
     N > 0,
     pick(N,X),
     NN is N-1,
     random_string(NN,L).

palindrome(L) :- reverse(L,L).

reverse(L,R) :-
     reverse(L,[],R).
reverse([],L,L).
reverse([A|B],S,R) :-
     reverse(B,[A|S],R).

twoBs([b,b|_]).
twoBs([_|L]) :-
     twoBs(L).

string_is_palindrome(N) :- string_is_palindrome(N,_).
string_is_palindrome(N,L) :- random_string(N,L),palindrome(L).

string_with_bb(N) :- string_with_bb(N,_).
string_with_bb(N,L) :- random_string(N,L),twoBs(L).

%len(5).
%evidence(string_is_palindrome(X)) :- len(X).
%query(string_is_palindrome(X)) :- len(X).

% m-067:ProbLog sad$ time problog strings.pl 
% string_is_palindrome(10):	0.03125   
% 
% real	1m4.226s
% user	1m1.496s
% sys	0m0.615s

% m-067:ProbLog sad$ time problog strings.pl 
% string_with_bb(10):	0.859375  
% 
% real	0m39.693s
% user	0m39.142s
% sys	0m0.253s

% m-067:ProbLog sad$ time problog strings.pl 
% string_with_bb(10):	0.75      (with evidence(string_is_palindrome(X)) :- len(X))
% 
% real	1m21.480s
% user	1m20.587s
% sys	0m0.397s
