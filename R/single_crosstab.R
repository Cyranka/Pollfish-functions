#' Creating a single crosstab from a pollfish file
#'
#' Function that creates the crosstabulation between the x_var and the y_var. Proportions displayed are row proportions
#' @param d_frame: Data frame created from a pollfish file
#' @param x_var: Variable that will represent the rows
#' @param y_var: Variable that will represent the columns
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' single_crosstab(x, "age", "Q4")


single_crosstab <- function(d_frame, x_var, y_var){
  
  
  zero_to_na <- function(x){ifelse(x == "0", NA,x)}
  df <- d_frame %>%
    select_(x_var, y_var) %>%
    drop_na() %>%
    filter_(paste(y_var, "!= 0")) %>%
    group_by_(x_var, y_var)  %>%
    tally() %>%
    ungroup() %>%
    group_by_(x_var) %>%
    mutate(percent = n/sum(n)) %>%
    select(-n) %>%
    spread_(y_var, "percent") %>%
    mutate_if(is_double, ~scales::percent(.,accuracy = .1))
  return(df)
}
