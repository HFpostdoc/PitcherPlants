###Analysis of the Hawley and Molly time series data
###MKLau 12 May 2014
###Taken from NSF-PRFB

###Summer Time Series Data from Aaron
###NOTE! Not published data

##Hawley and Molly bogs 1999 and 2000 time series
##META
##S. purpurea inquiline community
##Two bogs, Hawley (NW MA) and Molly (NW VT)
##Sampled weekly
##Three treatments (a1=open, a2=plug 3 weeks and a3=plug 6 weeks)
##Also 20 control plants, monitored as they became available for colonization
##a4=control
##length(table(a4$plant)[table(a4$plant)!=0])
##Blaesoxipha = Fletcherimyia fletcheri
##Invertebrates in Freshwater Wetlands of North America: Ecology and Management 
##edited by Darold P. Batzer, Russell B. Rader, Scott A. Wissinger
##page 414
##Also, see http://www.nku.edu/~dahlem/General%20Images/DahlemNacziPPlantSarcs.pdf
##And, http://www.nku.edu/~dahlem/PPlant/ppzflies.htm

##Structure of the food webs can be found in:
##Baiser et al. 2011
##Mouquet et al. 2008 (see citations inside too)
##Gotelli and Ellison 2006

##Model from Baiser et al. 2011

##1 Detritus-Prey 2 Bacteria 3 Metriocnemus 4 POM 5 Protozoa 6 Sarraceniopus 7 Habrotrocha 8 Wyeomyia 9 Fletcherimyia
library(sna)

A <- c(12, 13, 19, 25, 26, 27, 28, 29, 34, 42, 46, 47, 57, 58, 59, 78, 79, 89, 99)

webAssemble <- function(A,node.names=NULL){
  if (any(node.names==NULL)){
    node.names <- unique(unlist(strsplit(as.character(A),split='')))
  }
  aij <- do.call(rbind,strsplit(as.character(A),split=''))
  M <- matrix(0,nrow=length(node.names),ncol=length(node.names))
  rownames(M) <- colnames(M) <- node.names
  for (i in 1:nrow(aij)){
    M[as.numeric(aij[i,1]),as.numeric(aij[i,2])] <- 1
  }
  return(M)
}
sp.web <- webAssemble(A,node.names=c('Detritus-Prey', 'Bacteria', 'Metriocnemus', 'POM', 'Protozoa', 'Sarraceniopus', 'Habrotrocha', 'Wyeomyia', 'Fletcherimyia'))

sp.web

gplot(sp.web,displaylabels=TRUE,diag=TRUE,usearrows=TRUE,arrowhead.cex=0.25,vertex.cex=0.5,vertex.col='lightblue',label.cex=1,label.pad=2,edge.lwd=0.5)

###Total number of paths
sum(sp.web)

###Path coefficients from Baiser et al. 2011
###Work in Progress
## pc.web <- sp.web*0
## pc.web[1,2] <- -0.34
## pc.web[3,2] <- 0.48
## pc.web[5,2] <- -0.20
## pc.web[7,5] <- 0.10
## pc.web[,]

##Hawley 1999
h99 <- read.csv('./data/Hawley1999colonization.csv')
summary(h99)
head(h99)

##Get a list of species
names(table(h99$inquiline))

##Get total abundances of species
spp.ta <- tapply(h99$count,h99$inquiline,sum,na.rm=TRUE)
names(spp.ta)

##Make observation ids
oid <- paste(h99[,1],h99[,2],h99[,3],h99[,5],sep='_')

##Separate by treatment, plant, leavf and day
h99.l <- split(h99,oid)

##Build food web for known nodes for one pitcher (with maximum richness)
node.R <- unlist(lapply(h99.l,function(x) sum(sign(x$count),na.rm=TRUE)))
x <- h99.l[node.R==max(node.R)][[1]]

x <- h99.l[[1]]
x$inquiline <- as.character(x$inquiline)
                                        #"Blaesoxipha" 
x$inquiline[x$inquiline=='Blaesoxipha'] <- "Fletcherimyia"
                                        #"Habrotrocha" 
x$inquiline[x$inquiline=="Habrotrocha"]
                                        #"headcap" 
x$inquiline[x$inquiline=='headcap'] <- "Detritus-Prey"
                                        #"Metriocnemus" 
x$inquiline[x$inquiline=="Metriocnemus"]
                                        #"other" 
x$inquiline[x$inquiline=='other'] <- "POM"
                                        #"rotvol ml" 
                                        #"Sarraceniopus" 
x$inquiline[x$inquiline=="Sarraceniopus"]
                                        #"volume ml" 
                                        #"Wyeomyia"
x$inquiline[x$inquiline=="Wyeomyia"]
##Reduce flow network to extant species
spw.nodes <- rownames(sp.web)
obs.nodes <- x$inquiline[x$count>0&is.na(x$count)==FALSE]
obs.web <- sp.web
for (i in 1:length(spw.nodes)){
  if (spw.nodes[i]%in%obs.nodes==FALSE&spw.nodes[i]!='Bacteria'){
    obs.web[rownames(obs.web)==spw.nodes[i],] <- 0
    obs.web[,colnames(obs.web)==spw.nodes[i]] <- 0
  }
}
par(mfrow=c(1,2))
coord <- gplot(sp.web,displaylabels=TRUE,diag=TRUE,lwd=0.5)
gplot(obs.web,displaylabels=TRUE,diag=TRUE,lwd=0.5,coord=coord)

##Get webs?
ow.l <- list() #observed webs list
for (k in 1:length(h99.l)){
x <- h99.l[[k]]
x$inquiline <- as.character(x$inquiline)
                                        #"Blaesoxipha" 
x$inquiline[x$inquiline=='Blaesoxipha'] <- "Fletcherimyia"
                                        #"Habrotrocha" 
x$inquiline[x$inquiline=="Habrotrocha"]
                                        #"headcap" 
x$inquiline[x$inquiline=='headcap'] <- "Detritus-Prey"
                                        #"Metriocnemus" 
x$inquiline[x$inquiline=="Metriocnemus"]
                                        #"other" 
x$inquiline[x$inquiline=='other'] <- "POM"
                                        #"rotvol ml" 
                                        #"Sarraceniopus" 
x$inquiline[x$inquiline=="Sarraceniopus"]
                                        #"volume ml" 
                                        #"Wyeomyia"
x$inquiline[x$inquiline=="Wyeomyia"]
##Reduce flow network to extant species
spw.nodes <- rownames(sp.web)
obs.nodes <- x$inquiline[x$count>0&is.na(x$count)==FALSE]
obs.web <- sp.web
for (i in 1:length(spw.nodes)){
  if (spw.nodes[i]%in%obs.nodes==FALSE&spw.nodes[i]!='Bacteria'){
    obs.web[rownames(obs.web)==spw.nodes[i],] <- 0
    obs.web[,colnames(obs.web)==spw.nodes[i]] <- 0
  }
}
ow.l[[k]] <- obs.web
}
names(ow.l) <- names(h99.l)

###Intra-seasonal patterns
library('expm')

factors <- do.call(rbind,strsplit(names(ow.l),split='_'))
trt <- factors[,1];plant <- factors[,2];leaf <- factors[,3]
date <- as.Date(factors[,4],format='%Y%m%d')

##Total number of paths
owl.tnp <- unlist(lapply(ow.l,sum))

#Max path length
owl.pml <- unlist(lapply(ow.l,function(x) max(x%^%1000)))

#Combine data
pp.data <- data.frame(trt,plant,leaf,date,path.n=owl.tnp,path.max=owl.pml)
pp.data <- na.omit(pp.data)
                                        #pp.data <- pp.data[pp.data$path.n>0,]
                                        #Number of paths
plot(path.n~date,data=pp.data[pp.data$trt=='a1',],xlab='',ylab='Number of Paths',type='n')
points(path.n~date,data=pp.data[pp.data$trt=='a1',],cex=0.75)
                                        #Plot loess line
mpn.data <- tapply(pp.data$path.n,paste(pp.data$trt,pp.data$date),mean)
mpn <- as.numeric(mpn.data)
mpn.date <- as.Date(sapply(names(mpn.data),function(x) unlist(strsplit(x,split=' '))[2]))
mpn.trt <- as.character(sapply(names(mpn.data),function(x) unlist(strsplit(x,split=' '))[1]))
mpn.data <- data.frame(mpn,mpn.date,mpn.trt)
plot(mpn[mpn.trt=='a1']~mpn.date[mpn.trt=='a1'],xlab='',ylab='Number of Paths',ylim=c(0,4.5))
lines(spline(mpn[mpn.trt=='a1']~mpn.date[mpn.trt=='a1'],n=100))
points(mpn[mpn.trt=='a2']~mpn.date[mpn.trt=='a2'],xlab='',ylab='Number of Paths',ylim=c(0,4.5))
lines(spline(mpn[mpn.trt=='a2']~mpn.date[mpn.trt=='a2'],n=100))
points(mpn[mpn.trt=='a3']~mpn.date[mpn.trt=='a3'],xlab='',ylab='Number of Paths',ylim=c(0,4.5))
lines(spline(mpn[mpn.trt=='a3']~mpn.date[mpn.trt=='a3'],n=100))
                                        #Max Path Length
plot(path.max~date,data=pp.data[pp.data$trt=='a1',],xlab='',ylab='Maximum Path Length',type='n')
points(path.max~date,data=pp.data[pp.data$trt=='a1',],cex=0.75)
                                        #Plot loess line
max.data <- tapply(pp.data$path.max,paste(pp.data$trt,pp.data$date),mean)
mpn <- as.numeric(max.data)
mpn.date <- as.Date(sapply(names(max.data),function(x) unlist(strsplit(x,split=' '))[2]))
mpn.trt <- as.character(sapply(names(max.data),function(x) unlist(strsplit(x,split=' '))[1]))
mpn.data <- data.frame(mpn,mpn.date,mpn.trt)
plot(mpn[mpn.trt=='a1']~mpn.date[mpn.trt=='a1'],xlab='',ylab='Number of Paths',ylim=c(0,1))
lines(spline(mpn[mpn.trt=='a1']~mpn.date[mpn.trt=='a1'],n=100))


##Divide biomass among paths?

##Look at each pitcher sequentially
 coord <- gplot(sp.web,displaylabels=TRUE,diag=TRUE,lwd=0.5)
for (k in 1:length(h99.l)){
x <- h99.l[[k]]
x$inquiline <- as.character(x$inquiline)
                                        #"Blaesoxipha" 
x$inquiline[x$inquiline=='Blaesoxipha'] <- "Fletcherimyia"
                                        #"Habrotrocha" 
x$inquiline[x$inquiline=="Habrotrocha"]
                                        #"headcap" 
x$inquiline[x$inquiline=='headcap'] <- "Detritus-Prey"
                                        #"Metriocnemus" 
x$inquiline[x$inquiline=="Metriocnemus"]
                                        #"other" 
x$inquiline[x$inquiline=='other'] <- "POM"
                                        #"rotvol ml" 
                                        #"rot" ?????
                                        #"Sarraceniopus" 
x$inquiline[x$inquiline=="Sarraceniopus"]
                                        #"volume ml" 
                                        #"Wyeomyia"
x$inquiline[x$inquiline=="Wyeomyia"]
##Reduce flow network to extant species
spw.nodes <- rownames(sp.web)
obs.nodes <- x$inquiline[x$count>0&is.na(x$count)==FALSE]
obs.web <- sp.web
for (i in 1:length(spw.nodes)){
  if (spw.nodes[i]%in%obs.nodes==FALSE&spw.nodes[i]!='Bacteria'){
    obs.web[rownames(obs.web)==spw.nodes[i],] <- 0
    obs.web[,colnames(obs.web)==spw.nodes[i]] <- 0
  }
}
par(mfrow=c(1,2))
gplot(sp.web,displaylabels=TRUE,diag=TRUE,lwd=0.5,coord=coord)
gplot(obs.web,displaylabels=TRUE,diag=TRUE,lwd=0.5,coord=coord,main=names(h99.l)[k])
scan(n=1)
}


###Araujo Based Co-occurrence model
counts2com <- function(x,spn){
  foo <- matrix(0,nrow=1,ncol=length(spn))
  colnames(foo) <- spn
  for (j in 1:length(spn)){
    if (spn[j]%in%x[,4]){
      foo[1,j] <- x[x[,4]==spn[j],6]
    }
  }
  return(foo)
}

library(pbapply)
spn <- levels(h99$inquiline)[c(-9,-7)]
com <- pblapply(h99.l,counts2com,spn=)
com <- do.call(rbind,com)
com[is.na(com)] <- 0

###Hawley2000
h00 <- read.csv('~/projects/inquiline_network_evolution/data/Hawley2000colonization.csv')
h00[is.na(h00)] <- 0
                                        #remove observations with high protist counts
h00 <- h00[h00$protists<10000,]
colnames(h00)
wye <- h00[,8:14] #Weyomia
met <- h00[,15:22] #Metriocnemus
fle <- h00[,23:32] #Fletchermyia
sar <- h00[,33] #Sarraceniopus
hab <- h00[,34] #Habrotrocha
pro <- h00[,35] #protozoa
det <- h00[,36] #detritus-prey
wye <- apply(wye,1,sum)
met <- apply(met,1,sum)
fle <- apply(fle,1,sum)
h00. <- cbind("Detritus-Prey"=det,"Metriocnemus"=met,"Protozoa"=pro,"Sarraceniopus"=sar,"Habrotrocha"=hab,"Wyeomyia"=wye,"Fletcherimyia"=fle)
h00 <- cbind(h00,h00.)

##Does time predict community?
                                        #h00.date <- split(h00,h00$date)
A <- apply(h00,1,function(x) sum(as.numeric(x[37:43])))
R <- apply(h00,1,function(x) sum(sign(as.numeric(x[37:43]))))
com. <- cbind(h00.,ds=rep(1,nrow(h00.)))
summary(lm(A~h00$date))
summary(lm(R~h00$date))
plot(A~h00$date)
abline(lm(A~h00$date))
plot(R~h00$date)
abline(lm(R~h00$date))
adonis(com.~h00$date)

##Does plant id predict composition?
##Plant is poorly replicated


##Compared to vienna austria:
vienna <- read.csv('~/projects/nsf_prfb/data/vienna_flow.csv')
rownames(vienna) <- colnames(vienna)
Ti <- apply(vienna,2,sum)
gplot(vienna,displaylabels=TRUE,diag=TRUE,usearrows=TRUE,arrowhead.cex=0.35,vertex.cex=0.5,vertex.col='lightblue',label.cex=1,label.pad=2,edge.lwd=log(vienna)/5,uselen=TRUE,edge.len=0.001,gmode='digraph')
