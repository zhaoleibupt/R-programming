#download the dattsets

data.url<-"https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(data.url,"specdata.zip")
unzip("specdata.zip")

#the function is in the next
pollutantmean <- function(directory, pollutant, id=1:332) {
   files_list <- list.files(directory, full.names=TRUE)   
  dat <- data.frame()  
  dat1<-data.frame()
  for (i in 1:332) {                                
    dat <- rbind(dat, read.csv(files_list[i]))
  }
 for(i in id)
  {
    a<-dat[dat$ID==i,]
    dat1<-rbind(dat1,a)
  }
  mean(dat1[,pollutant],na.rm=T)
  }


