

##the function is in the next

corr<-function(directory,threhold=0){
  files_list <- list.files(directory,full.names=T)
  dat<-data.frame()
  cor1<-NULL
  for(i in 1:332)
  {
    dat<-na.omit(read.csv(files_list[i]))
    nob<-nrow(dat)
   if(nob>threhold) {
    a<-cor(dat$sulfate , dat$nitrate)
    cor1<-c(cor1, a)}
   }
  cor1
}

