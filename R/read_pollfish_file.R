#' Reading a pollfish file
#'
#' Function that reads a Pollfish file and automatically creates the region and income variables
#' @param enter_file: .xlsx file with the data
#' @keywords Pollfish
#' @export
#' @examples
#' read_pollfish_file("Pollfish_Survey.xls")



read_pollfish_file <- function(enter_file){
  
  ##This sheet contains income
  x <- readxl::read_excel(enter_file, sheet = "Individuals") %>%
    select(ID,`Income`) %>%
    magrittr::set_colnames(c("ID","income"))
  
  ##Fix income variable
  x <- x %>% mutate(income = case_when(
                  income %in% c("prefer_not_to_say") ~ NA_character_,
                  income %in% c("lower_i", "lower_ii") ~ "Under 50K",
                  TRUE ~ "Over 50K"))
  
  ##This sheet contains regions
  regions <- fbinfo::pop_geo %>% 
             select(state, region) %>%
             mutate(region = ifelse(region == "Western Overseas", "West", region))
    
  
  
  #Now we add merge the sheet with income and then with region
  y <- readxl::read_excel(enter_file, sheet = "Individuals Coded") %>%
    inner_join(x, by = c("ID" = "ID")) %>%
    left_join(regions, by = c("Area" = "state"))
  
  ##Order region, income
  y <- y %>% 
    mutate(income = factor(income, levels = c("Under 50K", "Over 50K")),
           region = factor(region, levels = c("South", "Northeast", "Midwest", "West", "Southwest")))
  
  ##Fix age and gender
  y <- y %>% 
    dplyr::rename(age = Age,
                  gender = Gender) %>%
    mutate(age = case_when(
      age == "> 54" ~ "Over 54",
      TRUE ~ age
    )) %>%
    mutate(age = factor(age, levels = c("18 - 24", "25 - 34","35 - 44","45 - 54", "Over 54"))) %>%
    mutate(gender = str_to_title(gender))
    
  y <- y %>%
    mutate_at(vars(matches("Q[1-9]{1,2}\\.*")),parse_number)
  return(y)

}