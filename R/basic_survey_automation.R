#' Automating basic data
#'
#' Function that will run all crosstabs for all specified questions with basic demographic information as rows
#' @param survey_file: Pollfish file to process
#' @param single_questions: Do you have a vector of single questions?
#' @param multiple_questions: Do you have a vector of multiple questions?
#' @param rank_questions: Do you have a vector of rank questions?
#' @keywords Pollfish, basic crosstab
#' @export
#' @examples
#' #Retrieve sample frequencies for rank choices
#' rank_choice <- c("Q7","Q8")
#' multiple_choice <- c("Q2", "Q9")
#' single_choice <-c("Q1","Q3","Q4","Q5","Q6", "Q11")
#' basic_survey_automation("Pollfish_Survey.xls",single_questions = single_choice,multiple_questions= multiple_choice,rank_questions = rank_choice)


basic_survey_automation <- function(survey_file, single_questions = NULL,multiple_questions = NULL, rank_questions = NULL){
  
  ##Create folders and copy file
  lapply(c("single_questions", "multiple_questions", "rank_questions"), function(i){
    dir.create(i, showWarnings = FALSE)
    file.copy(from = file.path(survey_file), to = i)
  })
  
  
  ##List of basic variables
  basic_variables <- c("region", "gender", "age", "income")
  
  ##Write single questions
  if(is.null(single_questions)){
    print("No single choice questions. Is this correct?")
  }else{
    setwd("single_questions/")
    
    retrieve_sample_and_questions(survey_file,single_questions,rank = FALSE, "single")
    x <- read_pollfish_file(survey_file)
    lapply(basic_variables, function(i)single_crosstabs_by_variable(x,i,single_questions))
    
    setwd("..//")
  }
  
  ##Write multiple questions
  if(is.null(multiple_questions)){
    print("No multiple choice questions. Is this correct")
  }else{
    setwd("multiple_questions/")
    
    retrieve_sample_and_questions(survey_file,multiple_questions,rank = FALSE, "multiple")
    x <- read_pollfish_file(survey_file)
    lapply(basic_variables, function(i)multiple_crosstabs_by_variable(x,i,multiple_questions))
    
    setwd("..//")
  }  
  
  ##Write rank questions
  if(is.null(rank_questions)){
    print("No rank questions. Is this correct?")
  }else{
    setwd("rank_questions/")
    
    retrieve_sample_and_questions(survey_file,rank_questions,rank = TRUE, "rank")
    
    x <<- read_pollfish_file(survey_file)
    print("File read")
    lapply(c(basic_variables,"sample"), function(i)rank_whole_question(x,i,rank_questions))
    
    setwd("..//")
  }  
  
  ##
  print("Basic crosstabs created")
}
