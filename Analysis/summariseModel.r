library(gplots)
library(lattice)
setwd("~/Documents/MPI/TurnTaking_CulturalEvolution/NewModel/Analysis")


d = read.table("../Results/testCapMaxCost.tab",sep='\t',header=TRUE)

d$learn_as_talking = F
d$turnEffect = as.factor(d$n_turns-d$n_conversations)

n = length(levels(factor(d$turnEffect)))
n.labels = c("20,1","10,2","5,4","2,10","1,20")

actual.proportions = c(0.111413043,0.157608696,0.663043478)
actual.proportions = actual.proportions/sum(actual.proportions)

# sum of squared error
d$fit = (abs(d$Prop_SOV - actual.proportions[3])^2)+(abs(d$Prop_SVO - actual.proportions[2])^2)+(abs(d$Prop_VSO - actual.proportions[1])^2)

xyplot(fit~turnEffect,groups=n_agents,d,type='a',auto.key=T,scales=list(x=list(labels=n.labels)),xlab='Conversations,Turns',ylab="Sum of Squares")

d.sel = d[d$n_agents==2 & d$learn_as_talking==0 & d$prob_random==0 & as.numeric(as.character(d$turnEffect)) < 0,]

vso=tapply(d.sel$Prop_VSO,d.sel$turnEffect,mean)
svo=tapply(d.sel$Prop_SVO,d.sel$turnEffect,mean)
sov=tapply(d.sel$Prop_SOV,d.sel$turnEffect,mean)






col3x = c("#e41a1c","#377eb8","#4daf4a")

barplot(cbind(c(vso[1],svo[1],sov[1]),c(vso[2],svo[2],sov[2]),actual.proportions),beside=F,names.arg=c("20 conversations\n1Turn","10 conversations\n2turns","Actual data"),col=col3x)
text(0.7,c(0.2,0.55,0.85),c("VSO",'SVO',"SOV"),cex=2)

plotmeans(Prop_VSO~turnEffect,data=d.sel,barcol=1,ylim=c(0,1),xaxt='n',xlab='Conversations,Turns',ylab="Proportion of runs",n.label=FALSE,p=0.99)
axis(1,1:length(levels(factor(d.sel$turnEffect))),labels=n.labels[1:length(levels(factor(d.sel$turnEffect)))])
par(new=TRUE)
plotmeans(Prop_SVO~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,box=FALSE,bty='n',col=2,barcol=2,ylim=c(0,1),p=0.99)
par(new=TRUE)
plotmeans(Prop_SOV~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,box=FALSE,bty='n',col=3,barcol=3,ylim=c(0,1),p=0.99)
legend(1,0.95,legend=c("VSO","SVO","SOV"),col=1:3,pch=1,lty=1)

points(rep(2.1,3),c(0.663043478,0.157608696,0.111413043),col=3:1,pch=8)



####
d.sel = d[d$n_agents==2 & d$learn_as_talking==0 & d$prob_random==0,]

plotmeans(Prop_VSO~turnEffect,data=d.sel,barcol=1,ylim=c(0,1),xaxt='n',xlab='Conversations,Turns',ylab="Proportion of runs",n.label=FALSE,p=0.99)
axis(1,1:length(levels(factor(d.sel$turnEffect))),labels=n.labels[1:length(levels(factor(d.sel$turnEffect)))])
par(new=TRUE)
plotmeans(Prop_SVO~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,box=FALSE,bty='n',col=2,barcol=2,ylim=c(0,1),p=0.99)
par(new=TRUE)
plotmeans(Prop_SOV~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,box=FALSE,bty='n',col=3,barcol=3,ylim=c(0,1),p=0.99)
legend(0.5,0.99,legend=c("VSO","SVO","SOV"),col=1:3,pch=1,lty=1)
abline(h=c(0.663043478,0.157608696,0.111413043),col=c('dark green','dark red','grey'),pch=8)


xyplot(Prop_SVO~prob_random,data=d,group=learn_as_talking,type='a',xlab='Noise',ylab='',auto.key=T)

xyplot(Prop_VSO~prob_random,data=d[d$turnEffect==-8,],group=n_agents,type='a',xlab='Noise',ylab='',auto.key=T)

xyplot(Prop_SOV~prob_random,data=d[d$turnEffect==1,],group=n_agents,type='a',xlab='Noise',col="#4daf4a",ylab='')
par(new=T)
xyplot(Prop_SVO~prob_random,data=d[d$turnEffect==1,],group=n_agents,type='a',xlab='Noise',col="#377eb8",ylab='')
xyplot(Prop_VSO~prob_random,data=d[d$turnEffect==1,],group=n_agents,type='a',xlab='Noise',col="#e41a1c",ylab='')



#####

d = read.table("../Results/BigSweep_SentFinalParticle2.tab",sep='\t',header=TRUE,stringsAsFactor=F)
d = d[d$run!='run',]
for(i in c(1:16,18,21)){
	d[,i] = as.numeric(d[,i])
}

d = d[d$probScale==0.1,]

d$turnEffect = as.factor(d$n_turns-d$n_conversations)

NoTurns = colSums(d[d$turnEffect==-19,2:7])
Turns = colSums(d[d$turnEffect==19,2:7])

ntx = rbind(NoTurns[1:3],NoTurns[4:6])
barplot(t(t(ntx)/colSums(ntx)),names.arg=c("V-","-V-","-V"),col=c("light green","dark green"))
text(0.7,0.5,"No\nsentence\nfinal\nparticle")

ttx = rbind(Turns[1:3],Turns[4:6])
barplot(t(t(ttx)/colSums(ttx)),names.arg=c("V-","-V-","-V"),col=c("light green","dark green"))
text(3.1,0.85,"Sentence\nfinal\nparticle",col='white')
text(3.1,0.4,"No\nsentence\nfinal\nparticle")

#######################


d = read.table("../../bigSweep_model4_initialInterrogative.tab",sep='\t',header=TRUE)
d$turnEffect = as.factor(d$n_turns-d$n_conversations)

NoTurns = colSums(d[d$turnEffect==-19,2:7])
Turns = colSums(d[d$turnEffect==19,2:7])

ntx = rbind(NoTurns[4:6],NoTurns[1:3])
barplot(t(t(ntx)/colSums(ntx)),names.arg=c("V-","-V-","-V"),col=c("light green","dark green"))
text(0.7,0.5,"No\nInitial\nQ",col='white')

ttx = rbind(WithQP=Turns[4:6],NoQP=Turns[1:3])
barplot(t(t(ttx)/colSums(ttx)),names.arg=c("V-","-V-","-V"),col=c("light green","dark green"))
text(0.7,0.88,"No\nInitial\nQ",col='white')
text(0.7,0.4,"Initial\nQ")

#######################


d = read.table("../../bigSweep_model4_SentFinalParticle_multi.tab",sep='\t',header=TRUE)
d$turnEffect = as.factor(d$n_turns-d$n_conversations)

NoTurns = colSums(d[d$turnEffect==-19,2:10])
Turns = colSums(d[d$turnEffect==19,2:10])

ntx = rbind(NoTurns[1:3],NoTurns[4:6],NoTurns[7:9])
barplot(t(t(ntx)/colSums(ntx)),names.arg=c("V-","-V-","-V"),col=rev(c("light green","dark green",'light blue')))
text(0.7,0.5,"No\nParticle")

ttx = rbind(No=Turns[1:3],Final=Turns[4:6],Initial=Turns[7:9])
barplot(t(t(ttx)/colSums(ttx)),names.arg=c("V-","-V-","-V"),col=rev(c("light green","dark green",'light blue')))
text(0.7,0.7,"Initial\nParticle")
text(0.7,0.3,"Final\nParticle",col='white')
text(0.7,0.1,"No\nParticle")


######

pdf("../Writeup/images/pdf/SingleRuns.pdf", width=6.5, height=3)
par(mfrow=c(1,3))
for(f in c("../../oneRunExample.tab","../../oneRunExample2.tab","../../oneRunExample3.tab")){
  example = read.table(f,sep='\t',header=TRUE)
  acol= "#e41a1c"
  
  cutoff = 50
  plot(example$Prop_VSO[1:cutoff],type='l',ylim=c(0,1),col=acol,xlim=c(1,cutoff),xlab='Generation',ylab='Proportion',lwd=3)
  
  bcol = "#377eb8"
  ccol = "#4daf4a"
  if(f=="../../oneRunExample3.tab"){
    ccol = "#377eb8"
    bcol = "#4daf4a"
  }
  if(f=="../../oneRunExample.tab"){
    legend(25,0.7, legend=c("SOV","SVO","VSO"),lty=1,col=c(ccol,bcol,acol), lwd=2)
  }
  
  points(example$Prop_SVO[1:cutoff],type='l',xaxt='n',yaxt='n',ylab='',xlab='',col=bcol,lwd=3)
  points(example$Prop_SOV[1:cutoff],type='l',xaxt='n',yaxt='n',ylab='',xlab='',col=ccol,lwd=3)
}
dev.off()


barplot(cbind(c(1,2,0)),col=c("#377eb8","#4daf4a","#e41a1c"))

par(mfrow=c(1,1))
########

# model fit

d.sel2 = d[d$learn_as_talking=="0" & d$turnEffect==19,]
numa = as.numeric(levels(factor(d.sel2$n_agents)))
pv = as.numeric(levels(factor(d.sel2$prob_random)))
m = matrix(nrow=length(numa),ncol=length(pv))
for(a in 1:length(numa)){
	for(p in 1:length(pv)){
		m[a,p] = mean(d.sel2[d.sel2$n_agents==numa[a] & d.sel2$prob_random==pv[p],]$fit)
	}
}
rownames(m) = numa
colnames(m) = pv
persp(numa,pv,m,theta=135,phi=30,shade=T,ticktype='detailed',xlab='Pop Size',ylab='Noise',zlab='\nSum of squared error',d=1.5)
