#' A Random ID function
#'
#' This function replaces a list of identifiers with random numeric ids and replaces the identifying variable with a variable called "RandomID" in the dataframe.  
#' @param x 
#' 
#' 


RandomID = function(x, identifier.variable, seed.value, id.digits) {
  
  if(missing(seed.value)) {seed.value=42}
  if(missing(id.digits)) {id.digits=5}
  
  #get random numbers
  set.seed(seed.value)
  
  link = as.data.frame(cbind(sample(as.numeric(paste(c(1, rep(0,id.digits-1)), collapse="")):as.numeric(paste(rep(9,id.digits), collapse="")), length(levels(factor(x[,which(names(x)==identifier.variable)]))), replace=FALSE), "idvar" = levels(factor(x[,which(names(x)==identifier.variable)]))))
  names(link)[1] = "ids"
  
  # Assign IDs to identifier.variable and remove identifier.variable , add duplicate flag
  x$RandomID = ""
  
  for (i in 1:length(x[,which(names(x)==identifier.variable)])) {
    x$RandomID[i] = as.character(link$ids[which(link$idvar==x[i,which(names(x)==identifier.variable)])])
  }
  
  #Move Random ID to first column, remove x500 variable
  data.randomid <- subset(x, select=c("RandomID", names(x)[-c(which(names(x)==identifier.variable), which(names(x)=="RandomID"))]))
  
  #Add flag for duplicates
  dupids = data.randomid$RandomID[which(duplicated(data.randomid$RandomID)==TRUE)]
  
  data.randomid$DuplicateFlag = ifelse(data.randomid$RandomID %in% dupids, 1, 0)
  

  cat("Data with Random ID has been saved to dataframe", paste("\"", "data.randomid", "\"", sep=""))
  
}




