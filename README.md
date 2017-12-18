# TurnTakingCulturalEvolution

Repository for model code in

Roberts, S. G., & Levinson, S. C. (2017). Conversation, cognition and cultural evolution: A model of the cultural evolution of word order through pressures imposed from turn taking in conversation. Interaction studies, 18(3), 402-429. doi:10.1075/is.18.3.06rob

Open link to paper: [http://www.mpi.nl/publications/escidoc-2449769](http://www.mpi.nl/publications/escidoc-2449769)

This paper outlines a first attempt to model the special constraints that arise in language processing in conversation, and to explore the implications such functional considerations may have on language typology and language change. In particular we focus on processing pressures imposed by conversational turn-taking and their consequences for the cultural evolution of  the structural properties of language. We present an agent-based model of cultural evolution where agents take turns at talk in conversation. When the start of planning for the next turn is constrained by the position of the verb the stable distribution of word orders evolves to match the actual distribution reasonably well. We suggest that the interface of cognition and interaction should be a more central part of the story of language evolution. 


The main simulation function is in `TT_model5.py`.  It includes a function *doRuns* which has the following parameters:

-  `n_agents`: Number of agents in a population
-  `n_turns`: Number of conversational turns in a conversation
-  `n_conversations`: Number of conversations per generation
-  `n_gen`: Number of generations
-  `n_runs`: Number of independent chain runs
-  `sentence_types`: A list of possible constructions that can be produced. Each construction is a string of elements.  The character V will be interpreted as a verb, and is used to calculate the production probabilities.  "X" will be interpreted as a particle and "?" as a question particle (see particleDiscount and initialQuestionBoost).

For example:

```
sentence_types = ["SOV","OSV","SVO","OVS","VOS","VSO"]
sentence_types = ["VSO","SVO","SOV"]
sentence_types = ["VSO","SVO","SOV","VSOX","SVOX","SOVX"]
sentence_types = ["VSO","SVO","SOV","?VSO","?SVO","?SOV"]
sentence_types = ["VSO","SVO","SOV","VSOX","SVOX","SOVX","XVSO","XSVO","XSOV"]
```

-  `prob_random`: Probability that an agent produces a random variant from the total set of possible sentence types. This is the noise parameter in the paper
-  `learn_as_talking`: Should constructions in conversations be added live to an agent's memory?
-  `print_generations`: If True, then the output prints data for each agent in each generation, as opposed to a summary of each generation.
-  `cap_max_cost`: Should the production cost be capped at 3?
-  `particleDiscount`: How long is a particle in comparison to a full word?
-  `initialQuestionBoost`: If True, add a 0.5 points boost to variants that begin with "?"
-  `probScale`: The alpha parameter from the paper that controls the relationship between verb distance and production probability.

The files `RunBigSweep.py` (and similar) show how to call the model to run through a sweep of parameter space.


# TurnTakingCulturalEvolution
