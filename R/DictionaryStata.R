#' A Stata Data Dictionary Function
#'
#' This function creates a .txt data dictionary from a Stata .dta read into R using the foreign package. It writes the variable labels and value labels stored as attributes within the R data object to an external file. 
#' @param x A dataframe read in using the "read.dta()" command from the foreign package. 
#' @param file The file name and location to which the data dictionary should be written. Defaults to "DataDictionary.txt" in the current working directory. 
#' @keywords data dictionary
#' @keywords metadata
#' @export
#' @examples
#' DictionaryStata()

DictionaryStata <- function(x, file="DataDictionary.txt") {
  
  attr(x, "value.labels") = list()
  for (i in 1:ncol(x)){
    if (attr(x, "val.labels")[i]!="") {
      attr(x, "value.labels")[i] <- attr(x, "label.table")[which(names(attr(x, "label.table"))==attr(data, "val.labels")[i])]}
    else { attr(x, "value.labels")[i] <- ""}}
  
  
  #set up a datatable of the variable names and labels
  value_labels <- attr(x, "value.labels")
  variable_labels <- attr(x, "var.labels")
  datatable <- as.data.frame(variable_labels)
  
  #initialize an empty tab-separated textfile to write the metadata to
  cat("\n", file=file, sep="\t")
  
  #loop through each variable in mydata and print the variable name, variable labels,
  #and value labels (if not blank) in the text file. 
  for (i in 1:ncol(x)){
    if (value_labels[i] != "") {
      #write a line with the variable name tab-separated from the variable label
      cat(names(x)[i], "\t", paste(datatable[i,]), file=file, append=TRUE)
      #create a temporary object that combines the value label numbers with the 
      #value labels, with "=" in the middle of the two - each row contains a value label
      a <- paste(cbind(rev(value_labels[[i]]))[,1], "=", row.names(cbind(rev(value_labels[[i]]))))
      #start a new line in the text file
      cat("\n", file=file, append=TRUE)
      #now, loop through each of the rows in "a" (each individual value label) and 
      #write it in a new indented line in the text file
      for (j in 1:length(a)){
        cat("\t", a[j], "\n", file=file, append=TRUE)
      }
      #add a line to separate the next variable
      cat("\n", file=file, append=TRUE)
    }
    #if the value labels are blank, just write a new line with the variable label
    else {
      cat(names(x)[i], "\t", paste(datatable[i,]), "\n\n", file=file, append=TRUE)
    }
  } 
}
