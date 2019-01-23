#' Creating a basic crosstab for a single rank question from a pollfish file
#'
#' Function that creates the crosstabulation between the x_var and the y_var for a single rank choice question. Output is a list with rank choices for all factors of x_var.
#' @param d_frame: Data frame created from a pollfish file
#' @param x_var: Variable that represent the categories
#' @param y_var: Variable that represent the ranking
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' rank_question(x, "age", "Q8")


rank_question <- function(d_frame, x_var, y_var){
  
  multiply_add_percent <- function(x){paste0(round(x*100,1),"%")}
  
  var_1 <- enquo(x_var)
  
  p <- d_frame %>% select(!!var_1, contains(paste0(y_var,"."))) %>%
    drop_na() %>% gather(choice, answer, -!!var_1) %>%
    filter(answer != 0) %>%
    group_by(!!sym(x_var),choice, answer) %>% tally()
  
  p <- p %>% ungroup() %>%
    mutate(choice = parse_number(str_replace_all(choice, paste0(y_var,"."),""))) %>%
    group_by(!!sym(x_var),choice) %>%
    mutate(percent = n/sum(n))
  
  p <- p %>% select(!!var_1, choice,answer,percent)
  
  ##Create a list
  my_levels <- p %>% pull(!!var_1) %>% unique() %>% parse_character()
  
  list_1 <- lapply(my_levels, function(i)filter(p,!!sym(x_var) == i) %>% ungroup() %>% select(-!!var_1))
  list_1 <- lapply(list_1, function(i) i %>% spread(answer,percent) %>% mutate_at(vars(matches("[0-9]{1,3}")),~scales::percent(.,accuracy = 1)))
  names(list_1) <- my_levels
  return(list_1)
}