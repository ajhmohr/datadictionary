#' A Data Dictionary Function
#'
#' This function creates a .txt data dictionary from a dataframe, list of variable labels, and list of value labels. It writes these things to a data dictionary file that can function as a codebook. 
#' @param x A dataframe
#' @param variable_labels A vector or list of variable labels. Should be the same length as ncol(x) and the labels should appear in the same order. 
#' @param value_labels A list of value labels. 
#' @param file The file name and location to which the data dictionary should be written. Defaults to "DataDictionary.txt" in the current working directory. 
#' @keywords data dictionary
#' @keywords metadata
#' @export
#' @examples
#' DataDictionary()


DataDictionary <- function(x, variable_labels, value_labels, file="DataDictionary.txt") {
  value_labels <- as.list(value_labels)
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
