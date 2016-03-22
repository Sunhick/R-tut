rm(list=ls())
setwd("/home/sunny/prv/tmp/interview/capital/babynames/")

file_list <- list.files(pattern="*.TXT")
count <- 0

for (file in file_list){
  print(paste('Processing ', file, ' ', count))
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.table(file, sep=",")
  }
  # if the merged dataset does exist, append to it
  else if (exists("dataset")){
    temp_dataset <-read.table(file, sep=",")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  count <- count + 1
}

rm(count)
rm(file)
rm(file_list)

attach(dataset)

# get popular name
k <- dataset[c('V4', 'V5')]
agg <- aggregate(cbind(V5)~V4, data=k,FUN=sum)
head(agg[order(agg$V5, decreasing = TRUE), ], n = 10)

rm(k)
rm(agg)

# get gender ambigious name for 2013
k <- dataset[c('V2', 'V3', 'V4', 'V5')]
M2013 <- k[k$V3 == 2013 & k$V2=='M',]
M2013 <- aggregate(cbind(V5)~V4, data=M2013, FUN=sum)

F2013 <- k[k$V3 == 2013 & k$V2=='F',]
F2013 <- aggregate(cbind(V5)~V4, data=F2013, FUN=sum)

tt <- merge(F2013, M2013, by="V4")
tt$Tot <- tt$V5.x+tt$V5.y
tt <- tt[c('V4', 'Tot')]
head(tt[order(tt$Tot, decreasing=TRUE), ], n=10)

# for year 1945
M2013 <- k[k$V3 == 1945 & k$V2=='M',]
M2013 <- aggregate(cbind(V5)~V4, data=M2013, FUN=sum)

F2013 <- k[k$V3 == 1945 & k$V2=='F',]
F2013 <- aggregate(cbind(V5)~V4, data=F2013, FUN=sum)

tt <- merge(F2013, M2013, by="V4")
tt$Tot <- tt$V5.x+tt$V5.y
tt <- tt[c('V4', 'Tot')]
head(tt[order(tt$Tot, decreasing=TRUE), ], n=10)

rm(tt)
rm(k)
rm(F2013)
rm(M2013)

# largest increase in popularity since 1980
k <- dataset[c('V3', 'V4', 'V5')]
p1980 <- k[k$V3 == 1980,]
p1980 <- aggregate(cbind(V5)~V4, data=p1980, FUN=sum)

p2014 <- k[k$V3 == 2014,]
p2014 <- aggregate(cbind(V5)~V4, data=p2014, FUN=sum)

m <- merge(p2014, p1980, by='V4')
m$change <- 100*(m$V5.x-m$V5.y)/m$V5.y
m <- m[c('V4', 'change')]
head(m[order(m$change, decreasing=TRUE), ], n=10)

rm(p1980)
rm(p2014)
rm(m)
rm(k)

# even larger increase
k <- dataset[c('V3', 'V4', 'V5')]
p1910 <- k[k$V3 == 1910,]
p1910 <- aggregate(cbind(V5)~V4, data=p1910, FUN=sum)

p2014 <- k[k$V3 == 2014,]
p2014 <- aggregate(cbind(V5)~V4, data=p2014, FUN=sum)

m <- merge(p2014, p1910, by='V4')
m$raise <- 100*(m$V5.x-m$V5.y)/m$V5.y
m <- m[c('V4', 'raise')]

head(m[order(m$raise, decreasing=TRUE), ], n=10)
tail(m[order(m$raise, decreasing=TRUE), ], n=10)

# detach dataset
detach(dataset)