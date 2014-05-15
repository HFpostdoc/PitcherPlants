###Function for loading the Hawley Bog 1999 dataset
###MKLau 15may2014

####Data are NOT published. Do not distribute
###(a1=open, a2=plug 3 weeks and a3=plug 6 weeks)

h99 <- read.csv('./data/Hawley1999colonization.csv')
h99 <- h99[,-ncol(h99)]
h99[is.na(h99)] <- 0
                                        #limit to open pitchers
h99 <- h99[h99$treatment=='a1',]
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
                                        #order by species name
h99.com <- h99.com[,order(colnames(h99.com))]
                                        #get environmental info
h99.env <- do.call(rbind,lapply(names(h99),function(x) unlist(strsplit(x,split=' '))))
colnames(h99.env) <- c('trt','plant','leaf','date')
                                        #make date an date vector
h99.env[,4] <- paste(substr(h99.env[,4],1,4),substr(h99.env[,4],5,6),substr(h99.env[,4],7,8),sep='-')
h99.date <- as.Date(h99.env[,4])
                                        #temporal pattern for a single species
head(h99.env);tail(h99.env)
h99.tpl <- paste(h99.env[,1],h99.env[,2],h99.env[,3]) #treatment plant leaf
h99.tpl.date <- split(h99.env[,4],h99.tpl)
h00.tpl.date <- lapply(h99.tpl.date,as.Date)
                                        #
head(h99.env)
print('Finished!')
