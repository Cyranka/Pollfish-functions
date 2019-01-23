#' Creating multiple crosstabs from a pollfish file
#'
#' Function that will run all single crosstabs for a single variable, save them in a list, and write them to file
#' @param d_frame1: Data frame created from a pollfish file
#' @param x_var1: Variable that will represent the rows
#' @param single_choice_columns: Vector with the code for all single choice variables
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' single_choice <-c("Q1","Q3","Q4","Q5","Q6", "Q11")
#' single_crosstabs_by_variable(x,"age",single_choice)


single_crosstabs_by_variable <- function(d_frame1, x_var1,single_choice_columns){
  z <- lapply(single_choice_columns, function(i)single_crosstab(d_frame1,x_var = x_var1, y_var = i))
  names(z) <- single_choice_columns
  writexl::write_xlsx(z, paste0(x_var1, "_single_choice", ".xlsx"))
  return(z)
}