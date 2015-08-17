rankhospital <- function(state, outcome,num="best") {
    data<- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Read outcome data
    if(!(state %in% (unique(data$State))))
        stop("invalid state")
    if(!(outcome %in% c("heart attack","heart failure","pneumonia")))
        stop("invalid outcome")
    if(outcome=="heart attack")
    {
        data[,11]<-as.numeric(data[,11])#把11列变成数字
        best<-data[data$State==state,] #选择state州
        a<-order(best[,11],best[,2]) #对heart attack 最低的医院进行排序
        best<-best[a,]
        hospital1<-best[,2] 
        if(num=="best")
            hospital<-hospital1[1]
        else if(num=="worst")
            hospital<-hospital1[length(na.omit(best[,11]))]
        else 
            hospital<-hospital1[num]
        
        return(hospital)
    }
    if(outcome=="heart failure")
    {
        data[,17]<-as.numeric(data[,17])#把11列变成数字
       
        best<-data[data$State==state,] #选择state州
        a<-order(best[,17],best[,2]) #对heart attack 最低的医院进行排序
        best<-best[a,]
        hospital1<-best[,2] 
        if(num=="best")
            hospital<-hospital1[1]
        else if(num=="worst")
            hospital<-hospital1[length(na.omit(best[,17]))]
        else 
            hospital<-hospital1[num]
        
        return(hospital)
    }
    if(outcome=="pneumonia")
    {
        data[,23]<-as.numeric(data[,23])#把11列变成数字
        best<-data[data$State==state,] #选择state州
        a<-order(best[,23],best[,2]) #对heart attack 最低的医院进行排序
        best<-best[a,]
        hospital1<-best[,2] 
        if(num=="best")
            hospital<-hospital1[1]
        else if(num=="worst")
            hospital<-hospital1[length(na.omit(best[,23]))]
        else 
            hospital<-hospital1[num]
        
        return(hospital)
    }
   
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}