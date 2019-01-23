#' Creating a basic crosstab for a multiple choice question from a pollfish file
#'
#' Function that creates the crosstabulation between the x_var and the y_var for a single multiple choice question. Proportions displayed are row proportions and refer to % of total answers.
#' @param d_frame: Data frame created from a pollfish file
#' @param x_var: Variable that will represent the rows
#' @param y_var: Variable that will represent the columns
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' x <- read_pollfish_file("Pollfish_Survey.xls")
#' multiple_question(x, "age", "Q2")


multiple_question <- function(d_frame, x_var, y_var){
  
  multiply_add_percent <- function(x){paste0(round(x*100,1),"%")}
  var_1 <- enquo(x_var)
  
  p <- d_frame %>% select(!!var_1, contains(paste0(y_var,"."))) %>%
    drop_na() %>% gather(choice, answer, -!!var_1) %>%
    filter(answer == 1) %>%
    group_by(!!sym(x_var),choice) %>% tally()
  
  p <- p %>% mutate(choice = parse_number(str_replace_all(choice, paste0(y_var,"."),""))) %>%
    arrange(!!sym(x_var), choice) %>%
    mutate(percent = n/sum(n)) %>%
    select(-n)
  
  p <- p %>% spread(choice, percent) %>%
    mutate_if(is_double, ~scales::percent(.,accuracy = .1))
  return(p)
}
