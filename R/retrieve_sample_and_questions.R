#' Creating sample frequencies and question names from a pollfish file
#'
#' Function that will run all rank questions crosstabs for a single variable, save them in a list, and write them to different files. There is a different file for each level of x_var
#' @param pollfish_file: Pollfish file to process
#' @param column_names: Columns to pull information from. Do not mix question type
#' @param rank: Do you want to retrieve information for rank questions. Default is FALSE
#' @param prefix_for_file: Prefix for the .xlsx file created
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' #Retrieve sample frequencies for rank choices
#' rank_choice <- c("Q7","Q8")
#' retrieve_sample_and_questions("Pollfish_Survey.xls",rank_choice,rank = TRUE,"rank")
#' 
#' #Retrieve sample frequencies for single choices
#' single_choice <-c("Q1","Q3","Q4","Q5","Q6", "Q11")
#' retrieve_sample_and_questions("Pollfish_Survey.xls",single_choice,rank = FALSE, "single")





retrieve_sample_and_questions <- function(pollfish_file, column_names,rank=FALSE,prefix_for_file){
  
  ##Function to get rid of parentheses
  remove_parentheses <- function(x){str_trim(str_remove_all(x,"\\(.*\\)"))}
  
  
  ##Sample totals
  work_sheets <- column_names
  work_sheets <- unlist(str_extract_all(work_sheets, "Q[0-9]{1,2}.*"))
  sample_answers <- lapply(work_sheets, function(i)read_excel(pollfish_file, sheet = i, skip = 1) %>%
                             select(-matches("Respondents")) %>%
                             rename_all(remove_parentheses))
  
  if(rank == FALSE){
        sample_answers <- lapply(sample_answers, function(i) i %>% mutate(row = row_number(),
                                                                    Answers = paste0(row,". ", Answers),
                                                                    Percent = scales::percent(Percent,accuracy = .1)) %>%
                                                                    select(-row))
  
  }else{
    sample_answers <- lapply(sample_answers, function(i) i %>% mutate(row = row_number(),
                                                                      Answers = paste0(row,". ", Answers)) %>%
                               select(-row))
  }
  names(sample_answers) <- column_names
  writexl::write_xlsx(sample_answers, paste0(prefix_for_file, "_sample_answers.xlsx"))
  
  ###
  questions <- sapply(work_sheets, function(i)read_excel(pollfish_file, sheet = i) %>% 
                        select(-matches("X__")) %>%
                        colnames())
  questions <- tibble(code = names(questions),
                      text = rtweet::plain_tweets(questions)) %>%
    mutate(text = paste0(code,". ",text))
  
  readr::write_csv(questions, paste0(prefix_for_file, "_text.csv"))
  
  return(sample_answers)
}