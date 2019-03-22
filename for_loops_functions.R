


pollutantmean<-function(directory,pollutant,id=1:332){
  
  files<-list.files(directory,full.names=TRUE)
  
  df<-data.frame()
  
  
  for(i in id){
    
    df<-rbind(df,read.csv(files[i]))
  }
  
  mean(df[,pollutant],na.rm=TRUE)
}




complete<-function(directory,id=1:332){
  
  files<-list.files(directory,full.names=TRUE)
  
  df<-data.frame()
  
  for (i in id){
    
    rd<-read.csv(files[i])
    nobs<-sum(complete.cases(rd))
    new<-data.frame(i,nobs)
    df<-rbind(df,new)
    
    
    
    
  }
  
  df
}




corr <- function(directory, threshold = 0) {
  files_full <- list.files(directory, full.names = TRUE)
  dat <- vector(mode = "numeric", length = 0)
  
  for (i in 1:length(files_full)) {
    moni_i <- read.csv(files_full[i])
    csum <- sum((!is.na(moni_i$sulfate)) & (!is.na(moni_i$nitrate)))
    if (csum > threshold) {
      tmp <- moni_i[which(!is.na(moni_i$sulfate)), ]
      submoni_i <- tmp[which(!is.na(tmp$nitrate)), ]
      dat <- c(dat, cor(submoni_i$sulfate, submoni_i$nitrate))
    }
  }
  
  dat
}



