#' Generate a SQL CASE statement from a mapping file
#'
#' This function constructs a CASE..WHEN,,THEN statement from a mapping file or
#' dataframe. It assumes that the first column of the mapping data contains the
#' original WHEN values, and the second column contains the THEN values (the
#' values to be mapped to.)
#'
#' @param inputfile Mapping dataframe OR path to the mapping file
#' @param header If reading a csv file, TRUE if the file includes a header row,
#' FALSE if it does not include a header row.
#' @return A string that represents the constructed CASE statement
#' @export
#' @examples 
#'  input <- Data_Frame <- data.frame(Training = c("Strength", "Stamina",
#'   "Other"), Duration = c(60, 30, 45))
#'  result <- casewhen(inputfile = input, header = TRUE)
casewhen <- function(inputfile = NULL, header = FALSE){
  if (is.null(inputfile) == TRUE) {
    stop("Please include a file path or an input dataframe.")
  }
  if (is.character(inputfile)) {
    mapping <- utils::read.csv(inputfile, header = header)
  } else {
    mapping <- inputfile
  }
  statement <- "\nCASE"
  for (i in 1:nrow(mapping)){
    statement = paste(statement, " WHEN ", "'", mapping[i, 1],
                      "'", " THEN ",
                      "'", mapping[i, 2], "'", "\n", sep="")
  }
  cat(statement)
  return(statement)
}

