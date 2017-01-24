#' A Qualtrics Preprocessing Function
#'
#' This function processes a Qualtrics data file that has been read into R (assumes a legacy View Results format). The first 10 variables automatically added by Qualtrics are renamed with their labels, labels are removed from the dataset and used to create a variable label file, a time taken variable is calculated based on the start and end times, and any identifying or otherwise unwanted variables are removed (specific variable names can be specified in function; see below). It writes a comma-separated (.csv) file of the processed data and a tab-separated (.txt) data dictionary of the variable labels to the working directory.
#' @param x A dataframe. Assumes dataframe is a Qualtrics .csv file that has been read into R with read.csv.
#' @param remove.std.var Logical. Whether the identifying variables added by Qualtrics (Name, EmailAddress, IPAddress, LocationLatitude, LocationLongitude, and LocationAccuracy) should be removed from the processed data. Defaults to TRUE.
#' @param remove.additional A character or vector of characters containing the names of additional variables to remove from the processed file. This can include other direct identifiers recorded in the Qualtrics survey.
#' @param data.file File name for the processed data file. String; should end in ".csv". Defaults to "ProcessedDataset.csv" in working directory.
#' @param label.file File name for the label file. String; should end in ".txt". Defaults to "VariableLabels.txt" in working directory.



QualtricsPreprocess <- function(x, remove.std.var=TRUE, remove.additional = "", data.file="ProcessedDataset.csv", label.file="VariableLabels.txt") {

  cat("Separating first row of labels from dataset", "\n\n")
  #remove first row of variable labels from data
  temp = x[-1,]

  #rename first 10 variables with label names
  labels = as.data.frame(t(x[1,]))
  labels[,1] = as.character(labels[,1])

  names(temp)[1:10] = labels[1:10,]
  row.names(labels)[1:10] = labels[1:10,]

  cat("Removing common html tags from the responses", "\n\n")

  #remove common html tags from responses
  for (i in 1:length(names(temp))) {
    temp[,i] = factor(gsub(pattern="<br>", replacement="", temp[,i]))
    temp[,i] = factor(gsub(pattern="</br>", replacement="", temp[,i]))
    temp[,i] = factor(gsub(patter="<br /> /", replacement="", temp[,i]))
  }


  #Calculate time on task
  temp$TimeTaken = difftime(strptime(temp$EndDate, "%Y-%m-%d %H:%M:%OS"),strptime(temp$StartDate, "%Y-%m-%d %H:%M:%OS"),  units=c("mins"))

  cat("Removing any standard or added variables", "\n\n")
  #If remove.std.var is true, then remove the standard identifying variables from Qualtrics data
  if (remove.std.var==TRUE){
  stdvars = c("Name", "EmailAddress", "IPAddress", "LocationLatitude", "LocationLongitude", "LocationAccuracy", "X")
  } else{stdvars = ""}

  temp1 = subset(temp, select=c(names(temp)[which(names(temp) %in% c(stdvars, remove.additional)==FALSE)]))

  cat("Writing out data file", "\n\n")

  #Write out .csv of processed data
  write.csv(temp1, data.file, row.names=FALSE)

  #Create data dictionary of variable labels
  for (i in names(temp1)) {
    if (i %in% row.names(labels)) {
      attr(temp1, "variable.labels")[which(names(temp1)==i)] = labels$`1`[which(row.names(labels)==i)]
    } else {
      attr(temp1, "variable.labels")[which(names(temp1)==i)] = ""
    }
     }

  #Add label for created time taken variable (confirm that it is the last column)
  attr(temp1, "variable.labels")[which(names(temp1)=="TimeTaken")] = "Time taken on survey in minutes"

  cat("Writing out label file", "\n\n")

  excel = t(as.data.frame(attr(temp1, "variable.labels")))
  write.table(cat(""), file=label.file, row.names=FALSE, col.names=FALSE, sep="\t")
  for (i in 1:ncol(temp1)) {
    cat(names(temp1)[i],"\t", paste(excel[,i]), "\n\n", file=label.file, append=T)
  }


  cat("Done")
}



