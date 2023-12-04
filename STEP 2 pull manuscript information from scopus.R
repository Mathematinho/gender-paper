library(tidyverse)
library(rscopus)

meta <- readxl::read_xlsx("data/meta.xlsx")

rscopus::set_api_key("add your scopus API")
                                                                                                                               


for (i in 1:nrow(meta)) {
  Query1 <- meta$query[i]
  print(paste("now query journal",i,meta$Journal.name[i]))
  file_path <- paste0("data/",meta$JCR.Abbreviation[i],".rds")
  if(!file.exists(file_path)){
    try(temp <- get_papers(Query1))
    if(exists('temp'))
    saveRDS(temp, file = paste0("data/",meta$JCR.Abbreviation[i],".rds"))
    remove('temp')
  }

}

########################################################################################
########################################################################################
library(tidyverse)

all_rds <- list.files("data","*.rds")

authors_raw <- lapply(setNames(all_rds,all_rds), function(xxx) readRDS(paste0("data/",xxx))$author) %>% 
  bind_rows(.id = "JCR.Abbreviation") %>%
  mutate(JCR.Abbreviation = gsub(".rds","",JCR.Abbreviation))

writexl::write_xlsx(authors_raw,"data/authors_raw.xlsx")

########################################################################################

all_rds <- list.files("data","*.rds")

papers <- lapply(setNames(all_rds,all_rds), function(xxx) readRDS(paste0("data/",xxx))$df) %>% 
  bind_rows(.id = "JCR.Abbreviation") %>%
  mutate(JCR.Abbreviation = gsub(".rds","",JCR.Abbreviation),
         subtype = subtypeDescription,
         entry_number = as.numeric(entry_number)) %>%
  rename(ttotal = `author-count.$`) %>%
  select(JCR.Abbreviation,entry_number,subtype,ttotal,`dc:title`,authkeywords)%>%
  distinct(JCR.Abbreviation,entry_number,subtype,ttotal,`dc:title`,authkeywords) 

writexl::write_xlsx(papers,"data/papers.xlsx")




#audit
nrow(meta)
list.files("data","*.rds") %>% length()




