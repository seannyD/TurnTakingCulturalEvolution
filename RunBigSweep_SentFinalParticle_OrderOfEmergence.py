from TT_model5 import *

sentence_types = ["VSO","SVO","SOV","VSOX","SVOX","SOVX","XVSO","XSVO","XSOV"]


print_generations = True
cap_max_cost = False
initialQuestionBoost = False
n_gen = 300
n_runs = 1000
#n_agents = 2
#n_turns = 20
#n_conversations = 1
#prob_random = 0
#learn_as_talking = False


for n_agents in [10]:
	for n_turns,n_conversations in [(10,2)]:
		for prob_random in [0]:
			for particleDiscount in [0.5]:
				for learn_as_talking in [False]:
					for probScale in [0.1]:
						doRuns(n_agents,n_turns,n_conversations,n_gen,n_runs,sentence_types,prob_random,learn_as_talking,print_generations,cap_max_cost,particleDiscount,initialQuestionBoost,probScale)