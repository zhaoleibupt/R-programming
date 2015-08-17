url<-"https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
download.file(url,"data")
unzip("data")


best <- function(state, outcome) {
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Read outcome data
    if(!(state %in% (unique(data$State))))
        stop("invalid state")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia")))
        stop("invalid outcome")
    if(outcome=="heart attack")
     {
        data[,11]<-as.numeric(data[,11])#把11列变成数字
        data<-na.omit(data)
        
        best<-data[data$State==state,] #选择state州
         a<-best[,11]==min(best[,11],na.rm=T) #选择11列等于最小值的取logical
        
        name<-best[a,][,2] #选择
        
        name1<-sort(name)[1]
        return(name1)
    }
    if(outcome=="heart failure")
    {
        data[,17]<-as.numeric(data[,17])#把11列变成数字
        data<-na.omit(data)
        best<-data[data$State==state,] #选择state州
        a<-best[,17]==min(best[,17],na.rm=T) #选择11列等于最小值的取logical
        name<-best[a,][,2] #选择
        name1<-sort(name)[1]
        return(name1)
    }
    if(outcome=="pneumonia")
    {
        data[,23]<-as.numeric(data[,23])#把11列变成数字
        data<-na.omit(data)
        best<-data[data$State==state,] #选择state州
        a<-best[,23]==min(best[,23],na.rm=T) #选择11列等于最小值的取logical
        name<-best[a,][,2] #选择
        name1<-sort(name)[1]
        return(name1)
    }
   
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}