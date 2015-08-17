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
        data[,11]<-as.numeric(data[,11])#��11�б������
        data<-na.omit(data)
        
        best<-data[data$State==state,] #ѡ��state��
         a<-best[,11]==min(best[,11],na.rm=T) #ѡ��11�е�����Сֵ��ȡlogical
        
        name<-best[a,][,2] #ѡ��
        
        name1<-sort(name)[1]
        return(name1)
    }
    if(outcome=="heart failure")
    {
        data[,17]<-as.numeric(data[,17])#��11�б������
        data<-na.omit(data)
        best<-data[data$State==state,] #ѡ��state��
        a<-best[,17]==min(best[,17],na.rm=T) #ѡ��11�е�����Сֵ��ȡlogical
        name<-best[a,][,2] #ѡ��
        name1<-sort(name)[1]
        return(name1)
    }
    if(outcome=="pneumonia")
    {
        data[,23]<-as.numeric(data[,23])#��11�б������
        data<-na.omit(data)
        best<-data[data$State==state,] #ѡ��state��
        a<-best[,23]==min(best[,23],na.rm=T) #ѡ��11�е�����Сֵ��ȡlogical
        name<-best[a,][,2] #ѡ��
        name1<-sort(name)[1]
        return(name1)
    }
   
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}