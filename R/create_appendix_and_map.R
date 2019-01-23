#' Creating an appendix and a map from a pollfish file
#'
#' Function that reads a Pollfish file and automatically creates frequency tables for the appendix and a map
#' @param pollfish_file: .xlsx file with the data
#' @keywords Pollfish
#' @export
#' @examples
#' create_appendix_and_map("Pollfish_Survey.xls")



create_appendix_and_map <- function(pollfish_file){
  x <- pollfishFunctions::read_pollfish_file(pollfish_file)
  
  
  ##Worksheets for appendix file
  states <- count(x, Area, region) %>%
    mutate(region = ifelse(is.na(region), "Northeast", parse_character(region))) %>%
    mutate(n = scales::comma(n)) %>%
    magrittr::set_colnames(c("state","region", "n"))
  
  
  region <- count(x, region) %>% mutate(region = ifelse(is.na(region), "Missing", parse_character(region))) %>%
    mutate(percent = scales::percent(n/sum(n)),
           n = scales::comma(n)) %>%
    magrittr::set_colnames(c("Region","Total", "Percent"))
  
  age <- count(x, age) %>% mutate(percent = scales::percent(n/sum(n)),
                                  n = scales::comma(n)) %>%
    magrittr::set_colnames(c("Age","Total", "Percent"))
  
  gender <- count(x, gender) %>% mutate(percent = scales::percent(n/sum(n)),
                                        n = scales::comma(n)) %>%
    magrittr::set_colnames(c("Gender","Total", "Percent"))
  
  income <- count(x, income) %>% mutate(income = ifelse(is.na(income), "Prefer not to say", parse_character(income))) %>%
    mutate(percent = scales::percent(n/sum(n)),
           n = scales::comma(n)) %>%
    magrittr::set_colnames(c("Income","Total", "Percent"))
  
  list_to_return <- list(states,region, age, gender, income)
  names(list_to_return) <- c("states_for_map","Region", "Age", "Gender", "Income")
  writexl::write_xlsx(list_to_return, "appendix_file.xlsx")
  
  ##Create map
  
  my_map <- pollfishFunctions::create_map("appendix_file.xlsx")
  my_map
  ggsave("plot.png", width=12, height=8, dpi=300)
  
  #Return list
  return(list(states,region, age, gender, income))
}
