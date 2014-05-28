###Function for loading the Molly Bog 1999 dataset
###MKLau 15may2014

m99 <- read.csv('../data/Molly1999colonization.csv')
m99 <- m99[,-ncol(m99)]
m99[is.na(m99)] <- 0
                                        #limit to open pitchers
m99 <- m99[m99$treatment=='a1',]
                                        #fix date error
m99$date[m99$date=='19997020'] <- '19990720'
                                        #split individual obs
m99 <- split(m99,paste(m99[,1],m99[,2],m99[,3],m99[,5]))
                                        #matricizing
m99.com <- matrix(0,nrow=length(m99),ncol=length(unique(m99[[1]]$inquiline)))
colnames(m99.com) <- unique(m99[[1]]$inquiline)
for (i in 1:length(m99)){
  for (j in 1:nrow(m99[[i]])){
    m99.com[i,colnames(m99.com)==m99[[i]][j,4]] <- m99[[i]][j,6]
  }
}
                                        #order by species name
m99.com <- m99.com[,order(colnames(m99.com))]
                                        #get environmental info
m99.env <- do.call(rbind,lapply(names(m99),function(x) unlist(strsplit(x,split=' '))))
colnames(m99.env) <- c('trt','plant','leaf','date')
                                        #make date an date vector
m99.env[,4] <- paste(substr(m99.env[,4],1,4),substr(m99.env[,4],5,6),substr(m99.env[,4],7,8),sep='-')
m99.date <- as.Date(m99.env[,4])
                                        #temporal pattern for a single species
head(m99.env);tail(m99.env)
m99.tpl <- paste(m99.env[,1],m99.env[,2],m99.env[,3]) #treatment plant leaf
m99.tpl.date <- split(m99.env[,4],m99.tpl)
m99.tpl.date <- lapply(m99.tpl.date,as.Date)
m99.tpl <- split(m99.com,m99.tpl)
m99.tpl <- lapply(m99.tpl,function(x) matrix(x,ncol=ncol(m99.com)))

                                        #temporal pattern for a single species
head(m99.env);tail(m99.env)
m99.tpl <- paste(m99.env[,1],m99.env[,2],m99.env[,3]) #treatment plant leaf
m99.tpl.date <- split(m99.env[,4],m99.tpl)
m99.tpl.date <- lapply(m99.tpl.date,as.Date)
m99.tpl <- split(m99.com,m99.tpl)
m99.tpl <- lapply(m99.tpl,function(x) matrix(x,ncol=ncol(m99.com)))
for (i in 1:length(m99.tpl)){colnames(m99.tpl[[i]]) <- colnames(m99.com)}
names(m99.tpl) <- names(m99.tpl.date)
                                        #
print(all(m99.tpl[[1]] == m99.com[1:nrow(m99.tpl[[1]]),]))
print('Finished!')
