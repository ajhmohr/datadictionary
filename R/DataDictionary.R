#' A Data Dictionary Function
#'
#' This function creates a .txt data dictionary from a dataframe, list of variable labels, and list of value labels. If no variable or value label lists are specified, it assumes these are stored in line with either the foreign or haven packages. It writes variable and value label metadata to a .txt data dictionary file that can function as a codebook. Optionally, you can choose to include summary information from each variable in the  data dictionary file with "include.summary=TRUE".
#' @param x A dataframe
#' @param variable_labels A vector or list of variable labels. Should be the same length as ncol(x) and the labels should appear in the same order. Defaults to "variable.labels" attribute of x or var_label(x).
#' @param value_labels A list of value labels. Defaults to "value.labels" attribute of x or val_labels(x)
#' @param include.summary Logical. Should a summary of the each variable (output of summary()) be included in the data dictionary? Defaults to FALSE.
#' @param file The file name and location to which the data dictionary should be written. Defaults to "DataDictionary.txt" in the current working directory.
#' @keywords data dictionary
#' @keywords metadata
#' @export
#' @examples iris #Create a data dictionary for iris dataset; varlab = c("Length of Sepal in centimeters", "Width of Sepal in centimeters", "Length of petal in centimeters", "Width of Petal in centimeters", "Species of iris"); vallab = c(rep("", ncol(iris))); DataDictionary(iris, variable_labels = varlab, value_labels=vallab, include.summary = TRUE)
#' DataDictionary()



DataDictionary <- function(x, variable_labels = variable_labels, value_labels = value_labels, include.summary=FALSE, file="DataDictionary.txt") {
  
  #foreign-style SPSS variable labels
  if (!is.null(attr(x, "variable.labels"))) {
    
    variable_labels <- attr(x, "variable.labels")
    
  } 
  
  #foreign-style Stata variable labels
  else if (!is.null(attr(x, "var.labels"))) {
      
      variable_labels <- attr(x, "var.labels")
      
    } 
  
  #haven-style SPSS or Stata variable labels
  else if (!is.null(unlist(sapply(x, attr, "label")))) {
      
      variable_labels <- sapply(x, attr, "label")
      
    }
    
  
  #foreign-style SPSS value labels
  if (!is.null(unlist(sapply(x, attr, "value.labels")))) {
    
    value_labels <- sapply(x, attr, "value.labels")
    
  } 
  
  #foreign-style Stata value labels
  else if (!is.null(attr(x, "label.table"))) {
    
    value_labels <- as.list(rep("", ncol(x)))
    names(value_labels) <- names(x)
    value_labels[which(names(value_labels) %in% names(attr(x, "label.table")))] <- attr(x, "label.table")
    
  }
    
  #haven-style SPSS or Stata variable labels
   else if (!is.null(unlist(sapply(x, attr, "labels", exact=TRUE)))) {
      
      value_labels <- sapply(x, attr, "labels", exact=TRUE)
      
    }
  
  
  datatable <- as.data.frame(variable_labels)

  #initialize an empty tab-separated textfile to write the metadata to
  cat("\n", file=file, sep="\t")

  ## START HERE - CHECK STATA
  #loop through each variable in mydata and print the variable name, variable labels,
  #and value labels (if not blank) in the text file.
  for (i in 1:ncol(x)){
    
    #if the value labels are blank or null, just write a new line with the variable label
     if (is.null(value_labels[[i]]) || value_labels[[i]] == "") {
      
       cat(names(x)[i], "\t", paste(datatable[i,]), "\n\n", file=file, append=TRUE)
      
      if (include.summary==TRUE) {
        capture.output(summary(x[,i]), file=file, append=T) }
       
       cat("\n", file=file, append=TRUE)
     }
    
    else {
      #write a line with the variable name tab-separated from the variable label
      cat(names(x)[i], "\t", paste(datatable[i,]), "\n\n", file=file, append=TRUE)

      #create a temporary object that combines the value label numbers with the
      #value labels, with "=" in the middle of the two - each row contains a value label
      a <- paste(cbind(rev(value_labels[[i]]))[,1], "=", row.names(cbind(rev(value_labels[[i]]))))
      #start a new line in the text file
      cat("\n", file=file, append=TRUE)
      #now, loop through each of the rows in "a" (each individual value label) and
      #write it in a new indented line in the text file
      for (j in 1:length(a)) {
        cat("\t", a[j], "\n", file=file, append=TRUE)
        }
      #add a line to separate the next variable
      cat("\n", file=file, append=TRUE)
      #if summary is TRUE, put in summary of data

       if (include.summary==TRUE) {
        capture.output(summary(x[,i]), file=file, append=T) } 

      cat("\n", file=file, append=TRUE)
      }
    }


}

