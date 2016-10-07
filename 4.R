rankall <- function (disease, num = "best"){
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(disease == "heart attack"){cnum = 11}
  else if(disease == "heart failure"){cnum = 17}
  else if(disease == "pneumonia"){cnum = 23}
  else { return(message("invalid outcome"))}
  
  f <- unique(factor(data$State))
  f <- sort(f)
  
  e <<- vector()
  
  for (i in 1:length(f)){
    
    a <- data[data$State == f[i],]
    
    a[,cnum] <- as.numeric(a[,cnum])
    
    filter <- complete.cases(a[,cnum])
    
    a <- a[filter,]
    
    d <<- a[order(a[,cnum],a$Hospital.Name),]
    
    if(num == "best"){n = 1}
    
    else if(num == "worst"){n = length(a[,cnum])}
    
    else {n = num}
    
    e <<- append(e,c(d$Hospital.Name[n],as.character(f[i])))
    
  }
  
  e <- as.data.frame(matrix(e,length(f),2,byrow = TRUE))
  
  colnames(e)<- c("hospital","state")
  rownames(e)<- f
    
  return(e)
}