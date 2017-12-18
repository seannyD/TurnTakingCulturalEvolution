setwd("~/Documents/MPI/TurnTaking_CulturalEvolution/NewModel/Analysis/")


d = read.delim("../Results/BigSweep_InitialDistributions_zeroRandom.tab", stringsAsFactors = F)

d = d[d$run!="run",]

for(x in c("n_conversations","n_turns",'n_runs','Prop_SVO','Prop_VSO',"Prop_SOV","probScale")){
  d[,x] = as.numeric(d[,x])
}

d$FinalWordOrder = "VSO"
d$FinalWordOrder[d$Prop_SVO>0.95] = "SVO"
d$FinalWordOrder[d$Prop_SOV>0.95] = "SOV"

WO = c("VSO","SVO","SOV")


d$initialDistAdv = sapply(d$initialTypeDist, function(X){
                      x =as.numeric(strsplit(X,"_")[[1]])
                      return(max(outer(x,x,"-")))
                    })
d$initialDistOrder = sapply(d$initialTypeDist, function(X){
  x =as.numeric(strsplit(X,"_")[[1]])
  return(paste(WO[which(x==max(x))],collapse=""))
})

d$turnEffect = as.factor(d$n_turns-d$n_conversations)

dx = d[d$probScale==0.1 & d$n_turns==20,]
  
table(dx$FinalWordOrder,dx$initialDistAdv,dx$initialDistOrder)

dx = d[d$probScale==0.1 & d$n_turns==20 & d$initialDistOrder=="VSOSVOSOV",]
vso=mean(dx$Prop_VSO)
svo=mean(dx$Prop_SVO)
sov=mean(dx$Prop_SOV)

col3x = c("#e41a1c","#377eb8","#4daf4a")

# 50 runs.  n.turns = 20, n.agents = 10, alpha = 0.1, random=0

pdf("../Writeup/images/pdf/InitialConditions.pdf")
par(mfrow=c(3,1), mar=c(5, 4, 1, 2) + 0.1)
for(wo in c("SOV","SVO","VSO")){
  dx = d[d$probScale==0.1 & d$n_turns==20 & d$initialDistOrder==wo,]
  vsox=tapply(dx$Prop_VSO,dx$initialDistAdv,mean)
  svox=tapply(dx$Prop_SVO,dx$initialDistAdv,mean)
  sovx=tapply(dx$Prop_SOV,dx$initialDistAdv,mean)
  
  prosx = unique(dx$initialTypeDist)
  prosx = sapply(prosx, function(X){
    paste(rev(strsplit(X,"_")[[1]]), collapse=" / ")
  })
  
  names.arg = c("0.3 / 0.3 / 0.3",prosx)
  barplot(cbind("0"=c(vso,svo,sov),rbind(vsox,svox,sovx)),
          names.arg = names.arg,
          col=col3x,
          ylab="")
  #text(4.3,0.4,wo, cex=3)
  if(wo=="VSO"){
    title(sub="Proportion of SOV / SVO / VSO in initial generation")
  }
  if(wo=="VSO"){
    text(4.3,0.9,"SOV",cex=1.5)
    text(4.3,0.65,"SVO",cex=1.5)
    text(4.3,0.35,"VSO",cex=1.5)
  }
}
dev.off()


# Without turn taking constraints

dx = d[d$probScale==0.1 & d$n_turns==1 & d$initialDistOrder=="VSOSVOSOV",]
vso2=mean(dx$Prop_VSO)
svo2=mean(dx$Prop_SVO)
sov2=mean(dx$Prop_SOV)

pdf("../Writeup/images/pdf/InitialConditions_noTTpressure.pdf")
par(mfrow=c(3,1), mar=c(5, 4, 1, 2) + 0.1)
for(wo in c("SOV","SVO","VSO")){
  dx = d[d$probScale==0.1 & d$n_turns==1 & d$initialDistOrder==wo,]
  vsox=tapply(dx$Prop_VSO,dx$initialDistAdv,mean)
  svox=tapply(dx$Prop_SVO,dx$initialDistAdv,mean)
  sovx=tapply(dx$Prop_SOV,dx$initialDistAdv,mean)
  
  prosx = unique(dx$initialTypeDist)
  prosx = sapply(prosx, function(X){
    paste(rev(strsplit(X,"_")[[1]]), collapse=" / ")
  })
  
  names.arg = c("0.3 / 0.3 / 0.3",prosx)
  barplot(cbind("0"=c(vso2,svo2,sov2),rbind(vsox,svox,sovx)),
          names.arg = names.arg,
          col=col3x,
          ylab="")
  #text(4.3,0.4,wo, cex=3)
  if(wo=="VSO"){
    title(sub="Proportion of SOV / SVO / VSO in initial generation")
  }
  if(wo=="VSO"){
    text(0.7,0.8,"SOV",cex=1.5)
    text(0.7,0.45,"SVO",cex=1.5)
    text(0.7,0.15,"VSO",cex=1.5)
  }
}
dev.off()