

##the function is in the next
complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory,full.names=T)
  nob<-vector(length=length(id))
  comp<-data.frame()
  for(i in id)
   {
    nob<-nrow(na.omit(read.csv(files_list[i])))
    dat<-data.frame(id=i,nobs=nob)
    comp<-rbind(comp,dat)
  }
  comp
}

