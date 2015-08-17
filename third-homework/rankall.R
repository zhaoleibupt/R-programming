rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia")))
        stop("invalid outcome")
    if(outcome=="heart attack")
    {
        data[,11]<-as.numeric(data[,11])#把23列变成数字
        data<-na.omit(data)
        data<-data[order(data$State),]
        data1<-split(data,data$State)
        lst<-list(NULL)
        for(i in 1:54){
            lst[[i]]<-data1[[i]][order(data1[[i]][,11],data1[[i]][,2]),][,2]
        }
        if(num=="best") num=rep(1,54)
        else if(num=="worst"){
            num<-tapply(data[,11],data$State,length)
        }
        else num=rep(num,54)
        b<-vector(length=54)
        for(i in 1:54){
            b[i]<-lst[[i]][num[i]]  
        }
        rankall<-data.frame()
        rankall<-cbind(b,unique(data$State))
    }
    
    if(outcome=="heart failure")
    {
        data[,17]<-as.numeric(data[,17])#把17列变成数字
        data<-na.omit(data)
        data<-data[order(data$State),]
        data1<-split(data,data$State)
        lst<-list(NULL)
        for(i in 1:54){
            lst[[i]]<-data1[[i]][order(data1[[i]][,17],data1[[i]][,2]),][,2]
        }
        if(num=="best") num=rep(1,54)
        else if(num=="worst"){
            num<-tapply(data[,17],data$State,length)
        }
        else num=rep(num,54)
        b<-vector(length=54)
        for(i in 1:54){
            b[i]<-lst[[i]][num[i]]  
        }
        rankall<-data.frame()
        rankall<-cbind(b,unique(data$State))
    }
    
    if(outcome=="pneumonia")
    {
        data[,23]<-as.numeric(data[,23])#把23列变成数字
        data<-na.omit(data)
        data<-data[order(data$State),]
        data1<-split(data,data$State)
        lst<-list(NULL)
        for(i in 1:54){
            lst[[i]]<-data1[[i]][order(data1[[i]][,23],data1[[i]][,2]),][,2]
        }
        if(num=="best") num=rep(1,54)
        else if(num=="worst"){
            num<-tapply(data[,23],data$State,length)
        }
        else num=rep(num,54)
        b<-vector(length=54)
        for(i in 1:54){
            b[i]<-lst[[i]][num[i]]  
        }
        rankall<-data.frame()
        rankall<-cbind(b,unique(data$State))
    }
    rankall<-as.data.frame(rankall)
    colnames(rankall)<-c("hospital","state")
    rownames(rankall)<-rankall$state
    return(rankall)
}