riverdata <- read.csv('rivers.csv',header = TRUE,sep = ';')
riverdata$V <- as.numeric(sub(",", ".", as.character(riverdata$V),fixed = TRUE))
riverdata$N <- as.numeric(sub(",", ".", as.character(riverdata$N),fixed = TRUE))
riverdata$Stock.Size <- as.numeric(sub(",", ".", as.character(riverdata$Stock.Size),fixed = TRUE))
colnames(riverdata) <- c('RiverName','V','N','position','Stock.Size','SizeCategory')

farmsites <- read.csv('farms.csv',header = TRUE,sep = ';')
farmsites$V <- as.numeric(sub(",", ".", as.character(farmsites$V),fixed = TRUE))
farmsites$N <- as.numeric(sub(",", ".", as.character(farmsites$N),fixed = TRUE))
farmsites$Stock <- as.numeric(sub(",", ".", as.character(farmsites$Stock),fixed = TRUE))
farmsites$SiteName <- as.character(farmsites$SiteName)
colnames(farmsites) <- c('SiteName','position','V','N','Stock')
save.image('Data.RData')

for( i in rownames(farmsites)){
  eval(parse(text=paste('inputs$tonn',i,"=farmsites[i,'Stock']",sep="")))
  save(inputs,file = "inputs.RData")
}

