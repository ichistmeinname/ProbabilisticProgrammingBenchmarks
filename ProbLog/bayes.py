from problog.program import PrologString
from problog.core import ProbLog
from problog import get_evaluatable
from problog.logic import Term, Constant

import sys

model = """
0.2 :: raining.
0.01 :: sprinkler :- raining.
0.04 :: sprinkler :- \+raining.
0.8 :: grassWet :- \+sprinkler, raining.
0.9 :: grassWet :- sprinkler, \+raining.
0.99 :: grassWet :- sprinkler, raining.

grassWetWhenRaining :- raining, grassWet.
query(grassWetWhenRaining).
"""
knowledge = get_evaluatable().create_from(model).evaluate()
print knowledge
