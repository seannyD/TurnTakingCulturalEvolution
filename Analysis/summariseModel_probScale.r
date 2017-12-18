library(gplots)
library(lattice)
setwd("~/Documents/MPI/TurnTaking_CulturalEvolution/NewModel/Analysis")


d = read.table("../Results/BigSweep_probScale.tab",sep='\t',header=TRUE)

d$learn_as_talking = F
d$turnEffect = as.factor(d$n_turns-d$n_conversations)

#n = length(levels(factor(d$turnEffect)))
#n.labels = c("20,1","10,2","5,4","2,10","1,20")

actual.proportions = c(0.111413043,0.157608696,0.663043478)
actual.proportions = actual.proportions/sum(actual.proportions)

# sum of squared error
d$fit = 
 (abs(d$Prop_SOV - actual.proportions[3])^2)
+(abs(d$Prop_SVO - actual.proportions[2])^2)
+(abs(d$Prop_VSO - actual.proportions[1])^2)

xyplot(fit~probScale,groups=turnEffect,d,type='a',auto.key=T,xlab='prob scale',ylab="Sum of Squares")

# draw graph showing how the proporitons of orders change with probScale

vsox=tapply(d$Prop_VSO,paste(d$turnEffect,d$probScale),mean)
svox=tapply(d$Prop_SVO,paste(d$turnEffect,d$probScale),mean)
sovx=tapply(d$Prop_SOV,paste(d$turnEffect,d$probScale),mean)

vsox2 = vsox[grepl('-8',names(vsox))]
svox2 = svox[grepl('-8',names(svox))]
sovx2 = sovx[grepl('-8',names(sovx))]


###########
#layout(matrix(c(1,2,3,4,5,6,7,8,9,10,12,12,12,11,11,11,11,11,11,11,11,11,11,12,12,12),nrow=2,byrow=T),heights=c(0.1,0.9))


colx = rainbow(2+length(sort(unique(d$probScale))))
pdf("../Writeup/images/pdf/Fig7_AlphaSettings.pdf", height=5,width=8)
layout(matrix(c(1,1,1,1,1,1,1,2,3,4,5,6,7,8,9,10,11,1,1,1,1,1,1,1,12,12,12,12,12,12,12,12,12,12),nrow=2,byrow=T),heights=c(0.1,0.9))

par(mar=c(5,4,4,2) + 0.1)
# plot of cost curves
plot(0:1~c(1,5),xlab='Distance between verbs',ylab='Processing cost',bg='white', type='n', main=paste('Settings for',expression(alpha),'parameter'))

for(i in 1:length(sort(unique(d$probScale)))){
  px = sort(unique(d$probScale))[i]
  
  # now displays processing cost rather than production ease
  pxx = 1 - (seq(1,5,by=0.1)^px)
  pxx = (pxx - min(pxx))/max(pxx - min(pxx))
  #	if(max((1:6))^px<1.1){
  #		print(px)
  #		pxx = 6+pxx
  #	}
  lines(seq(1,5,by=0.1),pxx,type='l',col=colx[i+2],xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
}


par(mar=c(0,1,2,1))
plot(1:10,xaxt='n',yaxt='n',bty='n',xlab='',ylab='',col='white')
for(i in 1:length(sort(unique(d$probScale)))){
	
	px = sort(unique(d$probScale))[i]
	pxx = 5 - (seq(1,5,by=0.1)^px)
	plot(seq(1,5,by=0.1),pxx,type='l',col=colx[i+2],xaxt='n',yaxt='n',bty='n',xlab='',ylab='')
}
plot(1:10,xaxt='n',yaxt='n',bty='n',xlab='',ylab='',col='white')
par(mar=c(5, 4, 2, 4) + 0.1)
col3x = c("#e41a1c","#377eb8","#4daf4a")
barplot(rbind(vsox2,svox2,sovx2),col=col3x,names.arg=sort(unique(d$probScale)),xlab=expression(alpha), ylab='Proportion of runs converging to each order')
text(4.3,c(0.2,0.55,0.85),c("VSO",'SVO',"SOV"),cex=1)
dev.off()

###########

# draw graph of sum of squares

ss = colSums(rbind((vsox - actual.proportions[1])^2,
(svox - actual.proportions[2])^2,
(sovx - actual.proportions[3])^2))

ss2 = data.frame(t(rbind(ss,matrix(unlist(strsplit(names(ss),' ')),ncol=length(ss)))))
names(ss2) = c('SSE','turnEffect','probScale')
ss2$SSE = as.numeric(as.character(ss2$SSE))
ss2$probScale = as.numeric(as.character(ss2$probScale))
ss2$turnEffect = as.factor(as.numeric(as.character(ss2$turnEffect)))

xyplot(SSE~turnEffect,groups=probScale,data=ss2,type='l',auto.key=T)

xyplot(SSE~probScale,groups=turnEffect,data=ss2,type='p',auto.key=T)



d.sel = d[d$n_agents==10 & d$learn_as_talking==0 & d$prob_random==0 & d$probScale==0.1 & d$turnEffect %in% c(-19,19),]

vso=tapply(d.sel$Prop_VSO,d.sel$turnEffect,mean)
svo=tapply(d.sel$Prop_SVO,d.sel$turnEffect,mean)
sov=tapply(d.sel$Prop_SOV,d.sel$turnEffect,mean)


col3x = c("#e41a1c","#377eb8","#4daf4a")
n.labels = c("20,1","10,2","5,4","2,10","1,20")

pdf("../Writeup/images/pdf/MainModelRes.pdf", width=6, height=5.5)
barplot(cbind(
	c(vso[1],svo[1],sov[1]),
	c(vso[4],svo[4],sov[4]),
	actual.proportions),beside=F,names.arg=c("20 conversations\n1 Turn","1 conversation\n20 turns","Actual isolate\ndistribution"),col=col3x)
text(0.7,c(0.2,0.55,0.85),c("VSO",'SVO',"SOV"),cex=2)
dev.off()

plotmeans(Prop_VSO~turnEffect,data=d.sel,barcol=1,ylim=c(0,1),xaxt='n',xlab='Conversations,Turns',ylab="Proportion of runs",n.label=FALSE,p=0.99)
axis(1,1:length(levels(factor(d.sel$turnEffect))),labels=n.labels[1:length(levels(factor(d.sel$turnEffect)))])
par(new=TRUE)
plotmeans(Prop_SVO~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,box=FALSE,bty='n',col=2,barcol=2,ylim=c(0,1),p=0.99)
par(new=TRUE)
plotmeans(Prop_SOV~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,box=FALSE,bty='n',col=3,barcol=3,ylim=c(0,1),p=0.99)
legend(1,0.95,legend=c("VSO","SVO","SOV"),col=1:3,pch=1,lty=1)

points(rep(2.1,3),c(0.663043478,0.157608696,0.111413043),col=3:1,pch=8)



####
d.sel = d[d$n_agents==10 & d$learn_as_talking==0 & d$prob_random==0 & d$probScale==0.1,]


pdf("../Writeup/images/pdf/ConvTurns.pdf", width=5,height=5)
plotmeans(Prop_VSO~turnEffect,data=d.sel,ylim=c(0,1),xaxt='n',xlab='Conversations,Turns',ylab="Proportion of runs",n.label=FALSE,p=0.99, col=col3x[1], barcol=col3x[1])

axis(1,1:length(levels(factor(d.sel$turnEffect))),labels=n.labels[1:length(levels(factor(d.sel$turnEffect)))])

par(new=TRUE)
plotmeans(Prop_SVO~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,bty='n',, col=col3x[2], barcol=col3x[2],ylim=c(0,1),p=0.99)
par(new=TRUE)
plotmeans(Prop_SOV~turnEffect,data=d.sel,xlab='',ylab='',xaxt='n',yaxt='n',n.label=FALSE,bty='n',, col=col3x[3], barcol=col3x[3],ylim=c(0,1),p=0.99)
legend(0.5,0.99,legend=c("SOV","SVO","VSO"),col=rev(col3x),pch=1,lty=1)
abline(h=c(0.663043478,0.157608696,0.111413043),col=rev(col3x),lty=2)
dev.off()

xyplot(Prop_SVO~prob_random,data=d,group=learn_as_talking,type='a',xlab='Noise',ylab='',auto.key=T)

xyplot(Prop_VSO~prob_random,data=d[d$turnEffect==-8,],group=n_agents,type='a',xlab='Noise',ylab='',auto.key=T)

xyplot(Prop_SOV~prob_random,data=d[d$turnEffect==1,],group=n_agents,type='a',xlab='Noise',col="#4daf4a",ylab='')
par(new=T)
xyplot(Prop_SVO~prob_random,data=d[d$turnEffect==1,],group=n_agents,type='a',xlab='Noise',col="#377eb8",ylab='')
xyplot(Prop_VSO~prob_random,data=d[d$turnEffect==1,],group=n_agents,type='a',xlab='Noise',col="#e41a1c",ylab='')



#####

d = read.table("../Results/BigSweep_SentFinalParticle2.tab",sep='\t',header=TRUE,stringsAsFactors=F)

d = d[d$run!='run',]
for(i in c(1:16,18,21)){
	d[,i] = as.numeric(d[,i])
}

d$n_turns = as.numeric(d$n_turns)
d$n_conversations = as.numeric(d$n_conversations)

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

# pdf("../Writeup/images/pdf/QParticle.pdf", width=7, height=4)
# par(mfrow=c(1,3))
# d = read.table("../Results/BigSweep_QParticle2.tab",sep='\t',header=TRUE, stringsAsFactors=F)
# d = d[d$run!='run',]
# for(i in 2:13){
# 	d[,i] = as.numeric(d[,i])
# }
# 
# d$turnEffect = as.factor(d$n_turns-d$n_conversations)
# 
# probscaleX = 1
# particleDiscountX = 0.5
# probRandomX = 0.05
# qboostX = "False"
# sel = d$probScale==probscaleX & d$particleDiscount==particleDiscountX & d$prob_random ==probRandomX & d$initialQuestionBoost==qboostX
# 
# 
# NoTurns = colSums(d[d$turnEffect==-19 & sel,2:7])
# Turns = colSums(d[d$turnEffect==19 & sel,2:7])
# 
# ntx = rbind(NoTurns[1:3],NoTurns[4:6])
# barplot(t(t(ntx)/colSums(ntx)),names.arg=c("V-","-V-","-V"),col=c("light green","dark green"), xlab='20 conversations\n1 Turn')
# text(0.7,0.5,"No\nInitial\nQ",col='white')
# 
# ttx = rbind(WithQP=Turns[1:3],NoQP=Turns[4:6])
# barplot(t(t(ttx)/colSums(ttx)),names.arg=c("V-","-V-","-V"),col=c("light green","dark green"), xlab='1 conversation\n20 turns')
# text(0.7,0.8,"No\nInitial\nQ",col='white')
# text(0.7,0.3,"Initial\nQ")
# 
# load("../EstimatesOfRealData/InitialQRealDataEstiamtes.rDat")
# 
# barplot(InitialQEstX,names.arg=c("V-","-V-","-V"),col=c("light green","dark green"), xlab='Actual data')
# text(0.7,0.88,"No\nInitial\nQ",col='white')
# text(0.7,0.4,"Initial\nQ")
# 
# dev.off()

########

q.fit = data.frame(probScale=NA,probRandom=NA,particleDiscount=NA,turnEffect=NA,qboost=NA,fit=NA,rank.fit=NA)

for(probScaleX in unique(d$probScale)){
	for(probRandomX in unique(d$prob_random)){
		for(particleDiscountX in unique(d$particleDiscount)){
			for(turnEffectX in unique(d$turnEffect)){
				for(qboostX in unique(d$initialQuestionBoost)){
			sel = d$probScale==probScaleX & d$particleDiscount==particleDiscountX & d$prob_random ==probRandomX & d$turnEffect==turnEffectX & d$initialQuestionBoost==qboostX

	NoTurns = colSums(d[sel ,2:7])

	ntx = rbind(NoTurns[1:3],NoTurns[4:6])
	ntx = t(t(ntx)/colSums(ntx))
	ntx[is.nan(ntx)] = 0

	fitx = sum((ntx - InitialQEstX)^2)
	fitx2 = adist(paste(order(ntx),collapse=''),paste(order(InitialQEstX),collapse=''))
	q.fit = rbind(q.fit,c(probScaleX,probRandomX,particleDiscountX,turnEffectX,qboostX,fitx,fitx2))
				}
			}
		}
	}
}
q.fit = q.fit[!is.na(q.fit$fit),]
q.fit$fit = as.numeric(q.fit$fit)
q.fit$rank.fit = as.numeric(q.fit$rank.fit)
q.fit[which(q.fit$fit==min(q.fit$fit)),]

q.fit[order(q.fit$fit),]

q.fit[order(q.fit$rank.fit),]




#######################


d = read.table("../Results/BigSweep_SentFinalParticle2.tab",sep='\t',header=TRUE, stringsAsFactors=F)
d = d[d$run!='run',]
for(i in 2:13){
	d[,i] = as.numeric(d[,i])
}
d$turnEffect = as.factor(d$n_turns-d$n_conversations)


par(mfrow=c(1,3))
probscaleX = 0.1#0.5#0.1
particleDiscountX = 0.5#1#0.5
probRandomX = 0#0.05#0
turnEffectX = 8#19#8
sel = d$probScale==probscaleX & d$particleDiscount==particleDiscountX & d$prob_random ==probRandomX 

NoTurns = colSums(d[d$turnEffect== -19 & sel ,2:10])
Turns = colSums(d[d$turnEffect==turnEffectX & sel,2:10])

pdf("../Writeup/images/pdf/SentenceFinalParticles.pdf", width=7, height=4)
par(mfrow=c(1,3))
ntx = rbind(NoTurns[1:3],NoTurns[4:6],NoTurns[7:9])
barplot(t(t(ntx)/colSums(ntx)),names.arg=c("V-","-V-","-V"),col=rev(c("light green","dark green",'light blue')),
        xlab='20 conversations\n1 Turn')
text(0.7,0.7,"Initial\nParticle")
text(0.7,0.25,"Final\nParticle",col='white')

ttx = rbind(No=Turns[1:3],Final=Turns[4:6],Initial=Turns[7:9])
barplot(t(t(ttx)/colSums(ttx)),names.arg=c("V-","-V-","-V"),col=rev(c("light green","dark green",'light blue')),
        xlab='2 conversations\n10 Turns')
text(0.7,0.7,"Initial\nParticle")
text(0.7,0.25,"Final\nParticle",col='white')
text(0.7,0.04,"No\nParticle")

load("../EstimatesOfRealData/QParticleRealDataEstiamtes.rDat")

barplot(estX2,names.arg=c("V-","-V-","-V"),col=rev(c("light green","dark green",'light blue')),
        xlab='Estimated\ndistribution')
text(0.7,0.7,"Initial\nParticle")
text(0.7,0.25,"Final\nParticle",col='white')
text(0.7,0.04,"No\nParticle")
dev.off()


###########

qpart.fit = data.frame(probScale=NA,probRandom=NA,particleDiscount=NA,turnEffect=NA,fit=NA,rank.fit = NA)

for(probScaleX in unique(d$probScale)){
	for(probRandomX in unique(d$prob_random)){
		for(particleDiscountX in unique(d$particleDiscount)){
			for(turnEffectX in unique(d$turnEffect)){
			sel = d$probScale==probScaleX & d$particleDiscount==particleDiscountX & d$prob_random ==probRandomX & d$turnEffect==turnEffectX

	NoTurns = colSums(d[sel ,2:10])

	ntx = rbind(NoTurns[1:3],NoTurns[4:6],NoTurns[7:9])
	ntx = t(t(ntx)/colSums(ntx))
	ntx[is.nan(ntx)] = 0

	fitx = sum((ntx - estX2)^2)
	fitx2 = adist(paste(order(ntx),collapse=''),paste(order(estX2),collapse=''))
	qpart.fit = rbind(qpart.fit,c(probScaleX,probRandomX,particleDiscountX,turnEffectX,fitx,fitx2))

			}
		}
	}
}
qpart.fit = qpart.fit[!is.na(qpart.fit$fit),]
qpart.fit$fit = as.numeric(qpart.fit$fit)
qpart.fit$rank.fit = as.numeric(qpart.fit$rank.fit)
qpart.fit[which(qpart.fit$fit==min(qpart.fit$fit)),]

qpart.fit[order(qpart.fit$fit),]
qpart.fit[order(qpart.fit$rank.fit),]


######
example = read.table("../../oneRunExample3.tab",sep='\t',header=TRUE)

cutoff = 50
plot(example$Prop_VSO[1:cutoff],type='l',ylim=c(0,1),col="#e41a1c",xlim=c(1,cutoff),xlab='Generation',ylab='Proportion',lwd=3)
points(example$Prop_SVO[1:cutoff],type='l',xaxt='n',yaxt='n',ylab='',xlab='',col="#4daf4a",lwd=3)
points(example$Prop_SOV[1:cutoff],type='l',xaxt='n',yaxt='n',ylab='',xlab='',col="#377eb8",lwd=3)

barplot(cbind(c(1,2,0)),col=c("#377eb8","#4daf4a","#e41a1c"))
########

# model fit

d.sel2 = d[d$learn_as_talking=="False" & d$turnEffect==19,]
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


#######