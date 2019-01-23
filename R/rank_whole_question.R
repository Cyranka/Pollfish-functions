#' Creating rank crosstabs from a pollfish file
#'
#' Function that will run all rank questions crosstabs for a single variable, save them in a list, and write them to different files. There is a different file for each level of x_var
#' @param d_frame1: Data frame created from a pollfish file
#' @param x_var: Variable that will represent the rows
#' @param rank_variables: Vector with the code for all rank questions
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' rank_choice <- c("Q7","Q8")
#' rank_whole_question(x,"region", rank_choice)


rank_whole_question <- function(d_frame, x_var,rank_variables){
  
  var_1 <- enquo(x_var)
  
  retrieve_factors <- d_frame %>% 
    select(!!var_1) %>% drop_na() %>%
    pull(!!var_1) %>% unique() %>% parse_character()
  
  l <- lapply(rank_variables,function(i)rank_question(x,x_var, i))
  
  ##
  lapply(retrieve_factors, function(i){
    k <- map(l, i)
    names(k) <- rank_variables
    writexl::write_xlsx(k,paste0(str_to_lower(i),"_rank_questions.xlsx"))
  })
  
  return(retrieve_factors) 
}