from TT_model5 import *

sentence_types = ["VSO","SVO","SOV"]

particleDiscount = 0.5
print_generations = False
cap_max_cost = False
initialQuestionBoost = False
n_gen = 300
n_runs = 50
#n_agents = 2
#n_turns = 20
#n_conversations = 1
#prob_random = 0
#learn_as_talking = False

initial_distribution_choices =  [[1/3.0,1/3.0,1/3.0], 
								 [0.4,0.3,0.3],
								 [0.3,0.4,0.3],
								 [0.3,0.3,0.4],
								 [0.6,0.2,0.2],
								 [0.2,0.6,0.2],
								 [0.2,0.2,0.6],
 								 [0.8,0.1,0.1],
 								 [0.1,0.8,0.1],
  								 [0.1,0.1,0.8],
  								 [1,0,0],
 								 [0,1,0],
  								 [0,0,1]
								 ]
								 
initial_distribution_choices =  [[1/3.0,1/3.0,1/3.0], 
								 [0.4,0.3,0.3],
								 [0.3,0.4,0.3],
								 [0.3,0.3,0.4],
								 [0.6,0.2,0.2],
								 [0.2,0.6,0.2],
								 [0.2,0.2,0.6],
 								 [0.8,0.1,0.1],
 								 [0.1,0.8,0.1],
  								 [0.1,0.1,0.8]
								 ]


printHeader = True
for n_agents in [10]:
	for n_turns,n_conversations in [(1,20),(10,2),(20,1)]:
		for prob_random in [0]:
			for learn_as_talking in [False]:
				#for probScale in [2,1,0.01,-1,-2]:
				for probScale in [0.1]:
					for initial_dist in initial_distribution_choices:
						doRuns(n_agents,n_turns,n_conversations,n_gen,n_runs,sentence_types,prob_random,learn_as_talking,print_generations,cap_max_cost,particleDiscount,initialQuestionBoost,probScale,initial_dist,printHeader)
						printHeader = False