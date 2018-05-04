#' Create .csv and Stata .do file
#'
#' This function creates a .csv file and a .do file from a dataframe in R to read into Stata.The data object in R should have variable labels specified in the attribute "variable.labels" and value labels in a list attribute "value labels", with both attributes having the same length as the number of variables in the data frame. 
#' As a compatibility check for Stata, any periods (".") in variable names will be replaced with underscores ("_"), all NA values will be replaced with the standard missing value in Stata ("."), and all variables will be cohersed to character to avoid factor conversion issues with Stata. 
#' @param df A dataframe
#' @param codefile The file name of Stata .do file that will be written to read in the csv data and apply the variable and value labels. 
#' @param datafile The file name of the .csv Data file that will be created. 
#' @keywords stata
#' @keywords do file
#' @export
#' @examples iris #Create a Stata do file and csv for iris dataset; attr(iris, "variable.labels") = c("Length of Sepal in centimeters", "Width of Sepal in centimeters", "Length of petal in centimeters", "Width of Petal in centimeters", "Species of iris"); attr(iris, "value.labels") = c(rep("", ncol(iris))); write.Stata(iris, codefile="Irisreadin.do", datafile="Irisdata.csv")
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
  
  cat("Writing Syntax file...", "\n")
  cat("insheet using", adQuote(paste(getwd(), datafile, sep="/")), ", case names", "\n\n", file=codefile) 
  
  for (i in 1:ncol(df)) {
    if (attr(df, "variable.labels")[i] != "") {
      cat("label var", names(df)[i], adQuote(gsub("\"", "", attr(df, "variable.labels")[i])), "\n", file=codefile, append=T)}
    
    if (attr(df, "value.labels")[i] != "") { 
      cat("label define", paste(names(df)[i], "label", sep = "_"),  paste(attr(df, "value.labels")[[i]], adQuote(as.character(names(attr(df, "value.labels")[[i]]))), collapse=" "), "\n", file=codefile, append=T)
      cat("label values", names(df)[i], paste(names(df)[i], "label", sep = "_"), "\n", file=codefile, append=T)
    }	
  }
  cat("\n\n", "save", adQuote(paste(getwd(), gsub(".csv", ".dta", datafile), sep="/")), ", replace", "\n\n", file=codefile, append=T) 
  cat("Done")	
}


