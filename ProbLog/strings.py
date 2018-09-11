from problog.program import PrologString
from problog.core import ProbLog
from problog import get_evaluatable
from problog.logic import Term, Constant

import sys

model = """
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
"""

query1 = """
query(string_is_palindrome(N)) :- len(N).
"""

query2 = """
query(string_with_bb(N)) :- len(N).
"""

if (len(sys.argv) >= 2) :
    if (len(sys.argv) == 2) :
        n = sys.argv[1]
        len_n = "len(%s)." % n
        knowledge = get_evaluatable().create_from(model + len_n + query1).evaluate()
    else :
        n = sys.argv[2]
        len_n = "len(%s)." % n
        knowledge = get_evaluatable().create_from(model + len_n + query2).evaluate()

print knowledge
