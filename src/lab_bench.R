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
data(enaModelInfo)
select <- unlist(lapply(enaModels,function(x) 
                        ((all(is.na(x%v%'export')==FALSE)
                          &
                          all((is.na(x%v%'respiration')==FALSE))
                          )
                         )
                        )
                 )
select <- (select==TRUE&enaModelInfo=='trophic')
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

names(out)[out.cor<=0]

plot(out$'Lake Findley ',type='l')
plot(out$'Swartkops Estuary  15',type='l')

