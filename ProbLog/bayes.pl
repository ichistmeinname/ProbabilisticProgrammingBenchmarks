0.2 :: raining.
0.01 :: sprinkler :- raining.
0.4 :: sprinkler :- \+raining.
0.8 :: grassWet :- \+sprinkler, raining.
0.9 :: grassWet :- sprinkler, \+raining.
0.99 :: grassWet :- sprinkler, raining.

grassWetWhenRaining :- raining, grassWet.
query(grassWetWhenRaining).
