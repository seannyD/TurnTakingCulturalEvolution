library(gplots)
library(lattice)
setwd("~/Documents/MPI/TurnTaking_CulturalEvolution/NewModel/Analysis")


# Sentence final particle - generation by generation

dsp = read.table("../Results/BigSweep_SentFinalParticle_OrderOfEmergence.tab",sep='\t',header=TRUE, stringsAsFactors=F)

dsp$dominantOrder = apply(dsp[,3:11],1, function(X){
  max = max(X)
  max.w = which(X==max(X))
  if(length(max.w)==1){
    if(max > max(X[-max.w]* 1.1)){
      return(gsub("Prop_","",names(X))[max.w])
    } else {
      return("None")
    }
  } else{
    return("None")
  }
})
dsp$dominant.particle = "None"
dsp$dominant.particle[dsp$dominantOrder %in% 
                        c("XSOV","XSVO","XVSO")] = "Initial"
dsp$dominant.particle[dsp$dominantOrder %in% 
                        c("SOVX","SVOX","VSOX")] = "Final"

dsp$dominantOrder2 = gsub("X","",dsp$dominantOrder)

finalOrder = tapply(dsp$dominantOrder, dsp$run, tail,n=1)
dsp$finalOrder= finalOrder[as.character(dsp$run)]

dsp.Vfinal.pfinal = dsp[dsp$finalOrder=='SOVX',]
plotmeans(dominant.particle=='Final'~gen, data = dsp.Vfinal.pfinal, bars = F)
plotmeans(dominantOrder2=='SOV'~gen, data = dsp.Vfinal.pfinal, bars = F)

firstP = tapply(dsp.Vfinal.pfinal$dominant.particle, 
                dsp.Vfinal.pfinal$run,
                function(X){
                  min(which(X=="Final"))
                })
firstV = tapply(dsp.Vfinal.pfinal$dominantOrder2, 
                dsp.Vfinal.pfinal$run,
                function(X){
                  min(which(X=="SOV"))
                })

plot(firstP,firstV)
abline(0,1)

hist(firstV - firstP, breaks=30)
# Proportion of runs where V appears before P
sum(firstV<firstP) / length(firstP)
sum(firstV>firstP) / length(firstP)
sum(firstV==firstP) / length(firstP)


plotmeans(dsp.Vfinal.pfinal$dominant.particle=="Final"~dsp.Vfinal.pfinal$gen,
          bars=F, n.label = F)
par(new=T)
plotmeans(dsp.Vfinal.pfinal$dominantOrder2=="SOV"~dsp.Vfinal.pfinal$gen,
          bars=F, n.label = F)

##########

dsp.Vfirst.pfirst = dsp[dsp$finalOrder=='XVSO',]

firstP = tapply(dsp.Vfirst.pfirst$dominant.particle, 
                dsp.Vfirst.pfirst$run,
                function(X){
                  min(which(X=="Initial"))
                })
firstV = tapply(dsp.Vfirst.pfirst$dominantOrder2, 
                dsp.Vfirst.pfirst$run,
                function(X){
                  min(which(X=="VSO"))
                })

plot(firstP,firstV)
abline(0,1)

hist(firstV - firstP, breaks=30)
# Proportion of runs where V appears before P
sum(firstV<firstP) / length(firstP)
sum(firstV>firstP) / length(firstP)
sum(firstV==firstP) / length(firstP)



###########



dsp$dominantOrder.prev = c(NA,dsp$dominantOrder[1:length(dsp$dominantOrder)-1])
dsp$dominantOrder.prev[dsp$gen==0] = NA


# previous vertical -> current horizontal
tx = table(dsp$dominantOrder.prev, dsp$dominantOrder)
nameorder = c("None",gsub("Prop_","",names(dsp)[3:11]))
tx = tx[nameorder,nameorder]
tx = tx / (rowSums(tx))
round(tx,3)


library(xtable)
print(xtable(tx,digits=3))

tx["SOV","SOVX"]
tx["XSOV","SOV"]
tx["SVOX","SOVX"]

# The domiant order is 
tx["SOV","SOVX"] / tx["SVOX","SOVX"]
# times more likely to transition from
# SOV to SOVX than from SVOX to SOVX

# The domiant order is 
tx["VSO","XVSO"] / tx["XSVO","XVSO"]
# times more likely to transition from
# VSO to XVSO than from XSVO to XVSO



tx["SOV","XSOV"] / tx["SOV","SOVX"]
tx["VSO","XVSO"] / tx["VSO","VSOX"]


tx["VSO","XVSO"]
tx["XVSO","XSVO"]
tx["XVSO","XSOV"]


finalOrderT = table(finalOrder)[gsub("Prop_","",names(dsp)[3:11])]
finalOrderT[is.na(finalOrderT)] = 0
names(finalOrderT) = gsub("Prop_","",names(dsp)[3:11])

v.init = finalOrderT[c("VSO",'VSOX',"XVSO")]
v.init = v.init/sum(v.init)
v.med = (finalOrderT)[c("SVO",'SVOX',"XSVO")]
v.med = v.med/sum(v.med)
v.fin = (finalOrderT)[c("SOV",'SOVX',"XSOV")]
v.fin = v.fin/sum(v.fin)

barplot(cbind(v.init,v.med,v.fin))

# Prob of VSO language gaining
# initial p
tx["VSO",'XVSO'] / tx["VSO",'VSOX']

tx["SOV",'XSOV'] / tx["SOV",'SOVX']
  
VSOgainsInitalp = tx["VSO",'XVSO'] + tx["VSO",'XSVO']+ tx["VSO",'XSOV']
# Prob of VSO language gaining final p
VSOgainsFinalp = tx["VSO",'VSOX'] + tx["VSO",'SVOX']+ tx["VSO",'SOVX']
VSOgainsInitalp/VSOgainsFinalp


SOVgainsInitalp = tx["SOV",'XSOV'] + tx["SOV",'XSVO']+ tx["SOV",'XVSO']
SOVgainsFinalp = tx["SOV",'VSOX'] + tx["SOV",'SVOX']+ tx["SOV",'SOVX']
SOVgainsInitalp/SOVgainsFinalp


tx2 = tx
diag(tx2) = 0
barplot(tx2["VSO",], main='VSO')
barplot(tx2["SOV",],main='SOV')

barplot(tx2["XVSO",],main='XVSO')
barplot(tx2["XSOV",],main='XSOV')



