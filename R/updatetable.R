#' Generate a SQL UPDATE statement from a mapping file
#'
#' This function constructs an UPDATE statement from a mapping file or
#' dataframe. It assumes that the first column of the data contains the
#' key column and list of keys for the rows where the corresponding other columns
#' have to be updated. It also assumes that the header for the data includes the
#' column names. The function will generate one UPDATE statement for each row in
#' the data.
#'
#' @param inputfile Dataframe OR path to the mapping file
#' @param tablename Name of the SQL table
#' @return A string that represents the constructed UPDATE statement(s)
#' @export
#' @examples 
#'  input <- Data_Frame <- data.frame(Training = c("Strength", "Stamina",
#'   "Other"), Pulse = c(100, 150, 120), Duration = c(60, 30, 45))
#'  result <- updatetable(inputfile = input, tablename = "myTable")
updatetable <- function(inputfile = NULL, tablename = NULL){
  if (is.null(inputfile) == TRUE) {
    stop("Please include a file path or an input dataframe.")
  }
  if (is.null(tablename) == TRUE) {
    stop("Please include the name for the SQL table to be updated.")
  }
  if (is.character(inputfile)) {
    mapping <- utils::read.csv(inputfile)
  } else {
    mapping <- inputfile
  }
  statement <- ""
  columns <- colnames(mapping)
  for (i in 1:nrow(mapping)){
    statement <- paste(statement, "\nUPDATE ", tablename, "\n", "SET ",
                       sep = "")
    for (j in 2:length(columns)){
      if (is.numeric(mapping[i, columns[j]])){
        statement <- paste(statement, columns[j], " = ", mapping[i, columns[j]],
                           ", ", sep="")
      } else {
        statement <- paste(statement, columns[j], " = ", "'", mapping[i, columns[j]],
                           "', ", sep="")
        }
    }
    statement <- substring(statement, 1, nchar(statement)-2)
    if (is.numeric(mapping[i, columns[1]])){
      statement <- paste(statement, "\nWHERE ", columns[1], " = ",
                       mapping[i, columns[1]], "\n", sep = "")
    } else {
      statement <- paste(statement, "\nWHERE ", columns[1], " = ",
                         "'", mapping[i, columns[1]], ",", "\n", sep = "")
    }
  }
  statement <- paste(statement, "\n", sep="")
  cat(statement)
  cat("\n")
  return(statement)
}