setwd("~/Documents/MPI/TurnTaking_CulturalEvolution/NewModel/Analysis/")


d = read.delim("../Results/BigSweep_OrderOfEmergence.tab", stringsAsFactors = F)

d$convergence = d$Prop_VSO>0.9 |  d$Prop_SVO>0.9|  d$Prop_SOV>0.9
hist(tapply(d$convergence,d$run, function(X){
  min(which(X))
}))


d$dominantOrder = NA
d$dominantOrder[d$Prop_VSO > 0.5] = "VSO"
d$dominantOrder[d$Prop_SVO > 0.5] = "SVO"
d$dominantOrder[d$Prop_SOV > 0.5] = "SOV"

d[d$gen==0,]$dominantOrder = "None"

while(sum(is.na(d$dominantOrder))>0){
  missing = which(is.na(d$dominantOrder))
  d[missing,]$dominantOrder = d[missing-1,]$dominantOrder
}

d[d$gen==0,]$dominantOrder = "None"

types = c("None","VSO","SVO","SOV")
tx = as.vector(outer(types,types,paste))

transitions = 
  tapply(d$dominantOrder, d$run, function(X){
  as.vector(table(paste(X[1:(length(X)-1)],X[2:(length(X))]))[tx])
})

transitions = do.call(rbind, transitions) 

transitionsSum = colSums(transitions, na.rm=T)
#transitionsSum = transitionsSum / sum(transitionsSum)
names(transitionsSum) = tx
tM = matrix(transitionsSum, nrow=4)
rownames(tM) = types
colnames(tM) = types

tM/rowSums(tM)

(tM/rowSums(tM))[2:4,2:4]

library(xtable)
print(xtable(round((tM/rowSums(tM))[2:4,2:4]*100,2), caption = "x"),'latex')

transitionsSum = transitionsSum / sum(transitionsSum)


pos = c(VSO="0,0", SVO="1,1", SOV ="2,0")


plot(c(-1,3),c(-1,3), type='n')
for(i in 1:length(transitionsSum)){
  if(!grepl("None",names(transitionsSum[i]))){
    bits = strsplit(names(transitionsSum)[i]," ")[[1]]
    if(bits[1]!=bits[2]){
      p1 = as.numeric(strsplit(pos[bits[1]],",")[[1]])
      p2 = as.numeric(strsplit(pos[bits[2]],",")[[1]])
      arrows(p1[1] + rnorm(1, sd=0.1),
             p1[2] + rnorm(1, sd=0.1),
             p2[1]+ rnorm(1, sd=0.1),
             p2[2]+ rnorm(1, sd=0.1), lwd = transitionsSum[i]*4000)
    }
  }
}
for(i in 1:length(pos)){
  p1 = as.numeric(strsplit(pos[i],",")[[1]])
  text(p1[1],p1[2],names(pos)[i])
}