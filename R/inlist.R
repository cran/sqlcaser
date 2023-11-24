#' Generate a SQL IN statement from a mapping file
#'
#' This function constructs an IN statement from a mapping file or
#' dataframe. It assumes that the first column of the data contains the
#' list of values to check for.
#'
#' @param inputfile Dataframe OR path to the mapping file
#' @param header If reading a csv file, TRUE if the file includes a header row,
#' FALSE if it does not include a header row.
#' @return A string that represents the constructed CASE statement
#' @export
#' @examples 
#'  input <- Data_Frame <- data.frame(Training = c("Strength", "Stamina",
#'   "Other"))
#'  result <- inlist(inputfile = input, header = TRUE)
inlist <- function(inputfile = NULL, header = FALSE){
  if (is.null(inputfile) == TRUE) {
    stop("Please include a file path or an input dataframe.")
  }
  if (is.character(inputfile)) {
    mapping <- utils::read.csv(inputfile, header = header)
  } else {
    mapping <- inputfile
  }
  statement <- paste("\nIN(", "'", mapping[1,1], "'", sep = "")
  for (i in 2:nrow(mapping)){
    statement <- paste(statement, ", ", "'", mapping[i, 1], "'", sep="")
  }
  statement <- paste(statement, ")\n", sep="")
  cat(statement)
  cat("\n")
  return(statement)
}

