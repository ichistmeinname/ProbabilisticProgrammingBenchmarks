from problog.program import PrologString
from problog.core import ProbLog
from problog import get_evaluatable
from problog.logic import Term, Constant

import sys

model = """
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
"""

query1 = """
query(palindrome(N)) :- len(N).
"""

query2 = """
query(bb(N)) :- len(N).
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
