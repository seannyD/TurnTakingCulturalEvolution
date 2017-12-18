import random,copy,math


sentence_types = []
particleDiscount= None
print_generations = None
cap_max_cost = None
probScale = None
initialQuestionBoost = None

#sentence_types = ["SOV","OSV","SVO","OVS","VOS","VSO"]
#sentence_types = ["VSO","SVO","SOV"]
#sentence_types = ["VSO","SVO","SOV","VSOX","SVOX","SOVX"]
#sentence_types = ["VSO","SVO","SOV","?VSO","?SVO","?SOV"]
#sentence_types = ["VSO","SVO","SOV","VSOX","SVOX","SOVX","XVSO","XSVO","XSOV"]

#particleDiscount = 0.5

#print_generations = False

#cap_max_cost = False
#initialQuestionBoost = False

class Agent:

	def __init__(self):
		self.memory = []
		self.prob = []
	
	def learn(self, item):
		self.memory.append(item)
		px = {}
		for turnA in sentence_types:
			# planning boost due to time between verbs
			x = (len(turnA) - turnA.index("V")) + item.index("V")
			if initialQuestionBoost and turnA.startswith("?"):
				x += 0.5    # boost from question being at the beginning
			if x>3 and cap_max_cost:
				x = 3  #  production cost is maximum 3 (does this cancel question production cost?)
			# scale according to power function
			x = math.pow(x, probScale)
			px[turnA] = x
		self.prob.append(px)
	
	def learnMany(self, items):
		for i in items:
			self.learn(i)
			
	def produce(self):
		if len(self.memory)>0:
			# discount particles (production cost)
			problist = []
			
			for x in self.memory:
				xcost = float(len(x))
				xcost = xcost - (x.count("X") * particleDiscount) - (x.count("?") * particleDiscount)
				problist.append(xcost) # cost of being less likely to choose X or ?
			return self.memory[self.roulette_wheel(problist)]
			#return random.choice(self.memory)
		else:
			return random.choice(sentence_types)

	def roulette_wheel(self,scores):
		"""Given a list of scores, returns
		a position in that list randomly in proportion to its score"""
		total = sum(scores)
		r = random.uniform(0,total)
		total = 0
		for i in range(len(scores)):
			total += scores[i]
			if r < total:
				return i
	
	def respond(self,sentence): #(returns choice and whether it was random)
		# choose randomly occasionally, or if memory is empty
		if len(self.memory)==0 or random.random()<prob_random:
			return random.choice(sentence_types),1
		else:
			# choose form in proportion to cognitive availability
			return self.memory[self.roulette_wheel([x[sentence] for x in self.prob])],0

	def getProp(self):
		freq = [self.memory.count(x) for x in sentence_types]
		sumFreq = float(sum(freq))
		if sum(freq)>0:
			for i in range(len(freq)):
				if freq[i]>0:
					freq[i] = freq[i]/sumFreq
		return freq
		
def getProps(agents):
	return meanProps([a.getProp() for a in agents])

def meanProps(props):
	props_ret = []
	for i in range(len(props[0])):
		props_ret.append(sum([x[i] for x in props])/float(len(props)))
	return props_ret

def updateDom(props_ret,dominant):
	dom = max(props_ret)
	if dom > (len(sentence_types)-1)/float(len(sentence_types)):
		dom_s = sentence_types[props_ret.index(dom)]
		if len(dominant)>0:
			if dom_s != dominant[-1]:
				dominant.append(dom_s)
		else:
			dominant.append(dom_s)
	return dominant
			
def transmit_utterances(utt,n):
	new_gen = [Agent() for x in range(n)]
	random.shuffle(utt)
	for i in range(len(utt)):
		new_gen[i % len(new_gen)].learn(utt[i])
	return new_gen
	
def makeInitialDistribution(dist,stypes,nTotal):
	return sum([[x]*int(y*nTotal) for x,y in zip(stypes,dist)],[])
	


def doRuns(n_agents,n_turns,n_conversations,n_gen,n_runs,sentence_typesX,prob_randomX,learn_as_talking,print_generationsX=False,cap_max_costX=False,particleDiscountX=0.5,initialQuestionBoostX=False,probScaleX=0, initialTypeDistribution=None, printHeader=True):
	
	global sentence_types, print_generations, cap_max_cost, particleDiscount, initialQuestionBoost, probScale, prob_random
	sentence_types = sentence_typesX
	print_generations =print_generationsX
	cap_max_cost = cap_max_costX
	particleDiscount = particleDiscountX
	initialQuestionBoost = initialQuestionBoostX
	probScale = probScaleX
	prob_random = prob_randomX
	
	if initialTypeDistribution==None:
		initialTypeDistribution = [1/float(len(sentence_types)) for x in sentence_types]
	initial_dist_string = "_".join([str(x) for x in initialTypeDistribution])
	
	if printHeader:
		if print_generations:
			print "run	gen	"+"\t".join(["Prop_"+x for x in sentence_types])+"	n_agents	n_turns	n_conversations	n_gen	n_runs	prob_random	learn_as_talking	probScale	initialQuestionBoost	capMaxCost	particleDiscount	initialTypeDist"
		else:
			print "run	"+"\t".join(["Prop_"+x for x in sentence_types])+"	n_agents	n_turns	n_conversations	n_gen	n_runs	prob_random	learn_as_talking	probScale	initialQuestionBoost	capMaxCost	particleDiscount	initialTypeDist"
	runs_res = []
	for run in range(n_runs):
		
		current_gen = [Agent() for x in range(n_agents)]
		new_gen = [Agent() for x in range(n_agents)]
		# initialise current_gen
		# Get distribution of initial types
		#a.learnMany([random.choice(sentence_types) for x in range(n_conversations*n_turns)])
		type_dist = makeInitialDistribution(initialTypeDistribution,sentence_types,n_conversations*n_turns)
		# Initialise first generation with memory
		for a in current_gen:
			a.learnMany(type_dist)
		xs = range(n_agents) # used to ranomise pairings of agents
		xs2 = range(n_agents)
		dominant = []
		randomSentenceCounter = 0
		#print "RUN ------------------"
		for gen in range(n_gen):
			new_gen = [Agent() for x in range(n_agents)]
			#print "Gen",gen
			gen_utterances = []
			# randomly pair up agents
			random.shuffle(xs)
			random.shuffle(xs2)
			for conv in range(n_conversations):	
				#print len(random.choice(current_gen).memory)
				for pair in range(len(xs)/2):
					agentA = current_gen[xs[pair*2]]
					agentB = current_gen[xs[(pair*2)+1]]
					#overhearing_agent1 = new_gen[xs2[pair*2]]
					#overhearing_agent2 = new_gen[xs2[(pair*2)+1]]
					s1 = agentA.produce()
					gen_utterances += [s1]
					for turn in range(n_turns-1):
						s2,rs = agentB.respond(s1)
						#print s1,s2
						randomSentenceCounter += rs
						# add utterances to memory
						if learn_as_talking:
							agentA.learn(s2)
							agentB.learn(s1)
						# store utterances for learning later
						gen_utterances += [s2]
						#random.choice(new_gen).learnMany([s1,s2])
						#overhearing_agent1.learnMany([s1,s2])
						#overhearing_agent2.learnMany([s1,s2])
						s1 = s2
						# swap agent roles
						#agentA,agentB = agentB, agentA
						agentA = current_gen[xs[(turn+1) % 2]]
						agentB = current_gen[xs[turn % 2]]
			# work out proportions
			props_ret = getProps(current_gen)
			# work out dominant order
			dominant = updateDom(props_ret,dominant)
			if print_generations:
				print str(run)+"\t"+str(gen)+"\t"+"\t".join([str(x) for x in props_ret])+"\t"+"\t".join([str(x) for x in [n_agents,n_turns,n_conversations,n_gen,n_runs,prob_random,learn_as_talking,probScale,initialQuestionBoost,cap_max_cost,particleDiscount,initial_dist_string]])
			#print gen,conv,[str(x)[:3] for x in props_ret]
			# new generation
			current_gen = transmit_utterances(gen_utterances,len(current_gen))
			#current_gen = copy.deepcopy(new_gen)
			#current_gen = new_gen
			#runs_res.append(props_ret)
		#print dominant, randomSentenceCounter/float(n_gen)
		# last proportion
		runs_res.append(props_ret)
		if not print_generations:
			print str(run)+"\t"+"\t".join([str(x) for x in props_ret])+"\t"+"\t".join([str(x) for x in [n_agents,n_turns,n_conversations,n_gen,n_runs,prob_random,learn_as_talking,probScale,initialQuestionBoost,cap_max_cost,particleDiscount,initial_dist_string]])
	#print sentence_types
	#print meanProps(runs_res)


# for n_agents in [10]:
# 	for n_turns,n_conversations in [(1,20),(2,10),(10,2),(20,1)]:
# 		for prob_random in [0,0.05]:
# 			for learn_as_talking in [False]:
# 				for probScale in [2,1,0.5,1,0.5,0.1,0.01,-0.5,-1,-2]:
# 					doRuns(n_agents,n_turns,n_conversations,n_gen,n_runs,prob_random,learn_as_talking)

#print "run	"+"\t".join(["Prop_"+x for x in sentence_types])+"	n_agents	n_turns	n_conversations	n_gen	n_runs	prob_random	learn_as_talking"
#learn_as_talking = False
#prob_random=0
#for n_turns,n_conversations in [(1,20),(2,10),(5,4),(10,2),(20,1)]:
#	doRuns(2,n_turns,n_conversations,300,100,0,False)


# for example runs
#for n_agents in [10]:
#	for n_turns,n_conversations in [(10,2)]:
#		for prob_random in [0.01]:
#			for learn_as_talking in [False]:
#				doRuns(n_agents,n_turns,n_conversations,n_gen,1,prob_random,learn_as_talking)
