from TT_model5 import *

sentence_types = ["VSO","SVO","SOV"]

particleDiscount = 0.5
print_generations = True
cap_max_cost = False
initialQuestionBoost = False
n_gen = 300
n_runs = 50
#n_agents = 2
#n_turns = 20
#n_conversations = 1
#prob_random = 0
#learn_as_talking = False




n_agents = 10
n_turns,n_conversations = (20,1)
prob_random = 0
learn_as_talking = False
probScale = 0.1

printHeader = True
for run in range(500):
	doRuns(n_agents,n_turns,n_conversations,n_gen,n_runs,sentence_types,prob_random,learn_as_talking,print_generations,cap_max_cost,particleDiscount,initialQuestionBoost,probScale,None,printHeader)
	printHeader = False