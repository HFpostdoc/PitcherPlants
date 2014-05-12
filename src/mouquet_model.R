###Analysis of the Mouquet 2008 P-plant inq. web
###MKLau 12 May 2014
###This was from pre-liminary analyses for NSF-PRFB

##Ascendency of Mouquet food web
##Mouquet et al. 2008
table1 <- t(read.csv('../data/mouq_inq_values.txt'))
math.values <- data.frame(t(table1))
colnames(math.values) <- sapply(colnames(t(table1)),function(x) unlist(strsplit(x,split='_'))[1])
                                        #build model from Figure 3
inq.C <- matrix(0,nrow=10,ncol=10)
inq.N <- matrix(0,nrow=10,ncol=10)
rownames(inq.C) <- colnames(inq.C) <- c('ants','rain','detritus','bacteria','protozoa','mosquito','sediment','N','plant','atmosphere')
rownames(inq.N) <- colnames(inq.N) <- c('ants','rain','detritus','bacteria','protozoa','mosquito','sediment','N','plant','atmosphere')
                                        #build models
attach(math.values)
                                        #carbon flows
inq.C[rownames(inq.C)=='ants',colnames(inq.C)=='detritus'] <- thetaA
inq.C[rownames(inq.C)=='detritus',colnames(inq.C)=='bacteria'] <- (uB*D*B)
inq.C[rownames(inq.C)=='detritus',colnames(inq.C)=='sediment'] <- (s*D)
inq.C[rownames(inq.C)=='bacteria',colnames(inq.C)=='detritus'] <- (mB*B)
inq.C[rownames(inq.C)=='bacteria',colnames(inq.C)=='protozoa'] <- (uP*B*P)
inq.C[rownames(inq.C)=='bacteria',colnames(inq.C)=='atmosphere'] <- (rB*B)
inq.C[rownames(inq.C)=='protozoa',colnames(inq.C)=='mosquito'] <- (uM*P)
inq.C[rownames(inq.C)=='protozoa',colnames(inq.C)=='detritus'] <- (mP*P)
inq.C[rownames(inq.C)=='protozoa',colnames(inq.C)=='atmosphere'] <- (rP*P)
inq.C[rownames(inq.C)=='mosquito',colnames(inq.C)=='atmosphere'] <- (rM*uM*P)
                                        #nitrogen flows
                                        #need to get C:N ratios for nodes
                                        #inq.N[rownames(inq.N)=='',colnames(inq.N)==''] <- 
                                        #load model into enaR for ascendency
library(enaR)
flow.nodes <- c(3,4,5,6,7)
flow <- inq.C[flow.nodes,flow.nodes]
input <- inq.C[rownames(inq.C)=='ants',flow.nodes]
export <- rep(0,nrow(flow))
respiration <- inq.C[flow.nodes,rownames(inq.C)=='atmosphere']
storage <- c(D,B,P,Mo,s)
living <- c(FALSE,TRUE,TRUE,TRUE,FALSE)
inqC.net <- pack(flow=flow,input=input,output=(export+respiration),export=export,
                 respiration=respiration,storage=storage,living=living)
                                        #ENA
plot(inqC.net,displaylabels=TRUE,vertex.cex=log(storage,2),label.pad=1,
     mode='circle',edge.lwd=log(inqC.net%n%'flow'),uselen=TRUE,edge.len=0.1)
                                        #ascendency
inqC.asc <- data.frame(enaAscendency(inqC.net))
data(troModels)
tro.rm <- unlist(lapply(troModels,function(x) any(x%v%'export'!=0)))
troModels <- troModels[is.na(tro.rm)==FALSE]
tro.asc <- do.call(rbind,lapply(troModels,enaAscendency))
tro.asc <- data.frame(tro.asc)
tot.biomass <- unlist(lapply(troModels,function(x) sum(x%v%'storage')))
                                        #plot total biomass ~ ascendency
plot((tot.biomass)~tro.asc$ASC.CAP,pch=19,xlab='Ascendency',ylab='Total System Biomass',cex=1,font.lab=2,col='grey')
#plot(log(tot.biomass)~tro.asc$ASC.CAP,pch=19,xlab='Ascendency',ylab='Total System Biomass (log scale)',cex=0.75,font.lab=2)
plot(log(tot.biomass)~log(tro.asc$ASC),pch=19,xlab='Ascendency',
     ylab='Total System Biomass',cex=1,font.lab=2,col=grey(0.3),
     xlim=c(3,17))
points(log(inqC.asc$ASC),log(sum(inqC.net%v%'storage')),pch=19,cex=2)
                                        #histogram of asc.cap
hist(c(tro.asc$ASC.CAP,inqC.asc$ASC.CAP),border='white',breaks=10,
     col=grey(0.3),xlim=c(0.2,0.9),ylim=c(0,15),main='',xlab='',ylab='')
                                        #points(inqC.asc$ASC.CAP,1,pch=19)
arrows((inqC.asc$ASC.CAP-0.0095),3,(inqC.asc$ASC.CAP-0.0095),1.5,lwd=5,col='red')

detach(math.values)
                                        #fig 4
flow <- matrix(c(0,0,0,
                 5,0,0,
                 5,5,0),
               nrow=3)
input <- c(10,0,0)
export <- rep(0,nrow(flow))
respiration <- c(0,0,10)
storage <- c(10,10,10)
living <- c(TRUE,TRUE,TRUE)
test1 <- pack(flow=flow,
             input=input,
             output=(export+respiration),
             export=export,
             respiration=respiration,
             storage=storage,
             living=living)
flow <- matrix(c(0,0,0,
                 10,0,0,
                 0,10,0),
               nrow=3)
input <- c(10,0,0)
export <- rep(0,nrow(flow))
respiration <- c(0,0,10)
storage <- c(10,10,10)
living <- c(TRUE,TRUE,TRUE)
test2 <- pack(flow=flow,
             input=input,
             output=(export+respiration),
             export=export,
             respiration=respiration,
             storage=storage,
             living=living)
                                        #
enaAscendency(test2) - enaAscendency(test1)


