from problog.program import PrologString
from problog.core import ProbLog
from problog import get_evaluatable
from problog.logic import Term, Constant

import sys

model = """
0.2 :: raining.
0.01 :: sprinkler :- raining.
0.4 :: sprinkler :- \+raining.
0.8 :: grassWet :- \+sprinkler, raining.
0.9 :: grassWet :- sprinkler, \+raining.
0.99 :: grassWet :- sprinkler, raining.

grassWetWhenRaining :- raining, grassWet.
grassWetQuery :- grassWet.
"""
if (len(sys.argv) == 2) :
    n = sys.argv[1]
    if (n == "1") :
      query = "query(grassWetQuery)."
    if (n == "2") :
      query = "query(grassWetWhenRaining)."
    if (n == "3") :
      query = """
        evidence(grassWet,true).
        query(grassWetWhenRaining).
        """
    knowledge = get_evaluatable().create_from(model + query).evaluate()
    print knowledge
