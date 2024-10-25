#' The CSV Reading Function
#'
#' @param csv a csv file path
#'
#' @return a data frame containing a representation of the data in the file
#' @export
#' @importFrom utils read.table
#'
#' @examples
#' myread("MTBE.csv")
myread <- function(csv) {
  fl <- paste0("C:/Users/prest/OneDrive/Desktop/MATH4753/Labs/", csv)

  read.table(fl, header = TRUE, sep = ",")
}
