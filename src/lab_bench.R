###The Lab Bench 
###Pitcher Plant projects
###MKLau 13May2014

###How is ascendency affected by connectance?
##Load a model
##Randomly remove a flow
##Calculate connectence (links/spp^2)
##Calculate ascendency

library(enaR)

rm.node <- function(x){
  y <- x%n%'flow'
  if (sum(abs(y))!=0){
    rnd.r <- (1:nrow(y))[apply(abs(y),1,sum)!=0]
    if (length((1:ncol(y))[abs(y[rnd.r,])!=0])==1){
      rnd.r <- (1:nrow(y))[apply(abs(y),1,sum)!=0]
      rnd.c <- (1:ncol(y))[abs(y[rnd.r,])!=0]
    }else{
      rnd.r <- sample((1:nrow(y))[apply(abs(y),1,sum)!=0],1)
      rnd.c <- sample((1:ncol(y))[abs(y[rnd.r,])!=0],1)
    }
    y[rnd.r,rnd.c] <- 0
    x%n%'flow' <- y
  }
  return(x)
}

data(enaModels)
select <- unlist(lapply(enaModels,function(x) 
                        ((all(is.na(x%v%'export')==FALSE)
                          &
                          all((is.na(x%v%'respiration')==FALSE))))))
out <- list()
for (k in 1:length(select[select==TRUE])){
x <- enaModels[select][[k]]
x.name <- names(enaModels[select])[k]
rm.x <- x
asc <- 0
con <- 0
for (i in 1:(length((x%n%'flow')[(x%n%'flow')!=0]))){
  if (length((rm.x%n%'flow')[(rm.x%n%'flow')!=0])==1){
    asc[i] <- 0
    con[i] <- 0
  }else{
    asc[i] <- enaAscendency(rm.x)[2]
    con[i] <- enaStructure(rm.x)$ns[3]
    rm.x <- rm.node(rm.x)
  }
}
out[[k]] <- cbind(con,asc)
print(k)
}

names(out) <- names(enaModels[select])
dput(out,file='../results/CvsA.R')
###plot
k <- 1
plot(out[[k]],type='l',ylim=c(0,max(do.call(rbind,out)[,2])),xlim=c(0,max(do.call(rbind,out)[,1])))
for (k in 2:length(out)){
  lines(out[[k]])
}
out.cor <- unlist(lapply(out,function(x) cor(x)[1,2]))
hist(out.cor)
plot(density(out.cor))

###Preliminary Hawley analysis looking for temporal trends.
####Data are NOT published. Do not distribute
h99 <- read.csv('./data/Hawley1999colonization.csv')
h99 <- h99[,-ncol(h99)]
h99[is.na(h99)] <- 0
                                        #fix date error
h99$date[h99$date=='19997020'] <- '19990720'
                                        #split individual obs
h99 <- split(h99,paste(h99[,1],h99[,2],h99[,3],h99[,5]))
                                        #matricizing
h99.com <- matrix(0,nrow=length(h99),ncol=length(unique(h99[[1]]$inquiline)))
colnames(h99.com) <- unique(h99[[1]]$inquiline)
for (i in 1:length(h99)){
  for (j in 1:nrow(h99[[i]])){
    h99.com[i,colnames(h99.com)==h99[[i]][j,4]] <- h99[[i]][j,6]
  }
}
h99.env <- do.call(rbind,lapply(names(h99),function(x) unlist(strsplit(x,split=' '))))
colnames(h99.env) <- c('trt','plant','leaf','date')
                                        #make date an date vector
h99.env[,4] <- paste(substr(h99.env[,4],1,4),substr(h99.env[,4],5,6),substr(h99.env[,4],7,8),sep='-')
h99.date <- as.Date(h99.env[,4])
                                        #plot of species dynamics across sampling dates
pdf('./results/h99_time.pdf')
par(mfrow=c(3,3))
for (i in 1:ncol(h99.com)){
  plot(h99.com[,i]~h99.date,main=colnames(h99.com)[i])
  lines(spline(h99.com[,i]~h99.date))
}
dev.off()
