#' Creating multiple crosstabs from a pollfish file
#'
#' Function that will run all multiple choice crosstabs for a single variable, save them in a list, and write them to file
#' @param d_frame1: Data frame created from a pollfish file
#' @param x_var1: Variable that will represent the rows
#' @param multiple_choice_columns: Vector with the code for all multiple choice variables
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' multiple_choice <- c("Q2", "Q9")
#' multiple_crosstabs_by_variable(x,"region", multiple_choice)

multiple_crosstabs_by_variable <- function(d_frame1, x_var1, multiple_choice_columns){
  
  z <- lapply(multiple_choice_columns, function(i)multiple_question(d_frame1,x_var1, i))
  names(z) <- multiple_choice_columns

  writexl::write_xlsx(z, paste0(x_var1, "_multiple_choice", ".xlsx"))
  
  return(z)
}
