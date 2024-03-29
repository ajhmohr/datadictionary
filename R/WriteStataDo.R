#' Create .csv and Stata .do file
#'
#' This function creates a .csv file and a .do file from a dataframe in R to read into Stata.The data object in R should have haven-style labels; variable labels specified in the each variable's attribute "label" and value labels in a list attribute "labels".
#' As a compatibility check for Stata, any periods (".") in variable names will be replaced with underscores ("_"), all NA values will be replaced with the standard missing value in Stata ("."), and all variables will be cohersed to character to avoid factor conversion issues with Stata. 
#' @param df A dataframe
#' @param tempcodefile The file name of Stata .do file that will be written to read in the csv data and apply the variable and value labels. 
#' @param datafile The file name of the .csv Data file that will be created. 
#' @keywords stata
#' @keywords do file
#' @export
#' @examples iris #Create a Stata do file and csv for iris dataset; labels= c("Length of Sepal in centimeters", "Width of Sepal in centimeters", "Length of petal in centimeters", "Width of Petal in centimeters", "Species of iris"); for (i in 1:ncol(iris)){  attr(iris[,i], "label") <- labels[i]}; write.Stata(iris, codefile="Irisreadin.do", datafile="Irisdata.csv")
#' DataDictionary()




write.Stata <- function(df, codefile, datafile) {
  adQuote <- function(x){paste("\"", x, "\"", sep = "")}
  #write out dataset 
  cat("Modifying dataset for compatibility with Stata...", "\n")
  names(df) = gsub("\\.|-", "_", names(df))
  
  df1 = apply(df, 2, as.character)
  df1[is.na(df1)] <- "."
  
  cat("Writing Data file...", "\n")
  write.table(df1, file = datafile, sep="\t", eol="\r\n", row = FALSE)
  
  #make sure it is a data frame (and not a tibble) before parsing metadata
  if (!is.data.frame(df)) {
    df <- as.data.frame(df)
  }
  
  cat("Writing Syntax file...", "\n")
  #set up internal object to collect text and speed up write
  tempcodefile <- textConnection("codetext", "w")
  
  cat("import delimited using", adQuote(paste(getwd(), datafile, sep="/")), ", delim(\"\\t\") clear case(\"preserve\")", "\n\n", file=tempcodefile) 
  
  for (i in 1:ncol(df)) {
    if (!is.null(attr(df[,i], "label", exact = T))) {
      cat("label var", names(df)[i], adQuote(gsub("\"", "", attr(df[,i], "label", exact = T))), "\n", file=tempcodefile, append=T)}
    
    if (!is.null(attr(df[,i], "labels", exact = T))) { 
      cat("label define", paste(names(df)[i], "label", sep = "_"),  paste(attr(df[,i], "labels", exact = T), adQuote(as.character(names(attr(df[,i], "labels", exact = T)))), collapse=" "), "\n", file=tempcodefile, append=T)
      cat("label values", names(df)[i], paste(names(df)[i], "label", sep = "_"), "\n", file=tempcodefile, append=T)
    }	
  }
  cat("\n\n", "save", adQuote(paste(getwd(), gsub(".csv|.tsv|.txt", ".dta", datafile), sep="/")), ", replace", "\n\n", file=tempcodefile, append=T) 
  
  close(tempcodefile)
  
  cat(codetext, file=codefile, sep = "\n")
  
  cat("Done")	
}


