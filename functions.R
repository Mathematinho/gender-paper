library(dplyr)
library(rstudioapi)
library(stringr)
library(readxl)
library(rscopus)
library(gender)

get_papers <- function(qqq){
  
  ######################query all papers###################################
  completeArticle <- scopus_search (
    query = qqq, 
    view = "COMPLETE", 
    count = 24)
  
  alldata <- gen_entries_to_df(completeArticle$entries)
  
  alldata$author <- alldata$author %>% drop_na(`@_fa`) %>%
    group_by(entry_number) %>%
    mutate(First = gsub('\\w{1}\\.','',`given-name`) %>% word(1),
           rank = case_when(`@seq` == 1 ~ 'First Author',
                            `@seq` == n() ~ 'Senior Author'))
  
  ######################return###################################
  return(alldata)
}






