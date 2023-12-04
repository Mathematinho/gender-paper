library(tidyverse)
library(readxl)

authors <- read_xlsx('data/authors.xlsx')

#pad gender
gendered <- read_xlsx('data/gendered.xlsx')
authors <- authors %>% left_join(gendered)

name_database <- c("ssa","kantrowitz","ipums", "napp")
######################gender###################################
library(gender)
for(nnn in name_database) {
  need_gender <- authors %>% filter(is.na(gender) & First != "") %>% pull(First) %>% unique() 
  print(paste(length(need_gender),"need gender, now get from",nnn))
  getgender<-gender(need_gender,method = nnn)
  if (drop_na(getgender) %>% nrow() > 0){
    authors <- full_join(authors,getgender %>% rename(First = name),by="First") %>%
      rowwise() %>%
      mutate(gender = coalesce(gender.x,gender.y)) %>%
      select(-contains(".x") & -contains(".y") & -contains("proportion"))
  }
}

######################genderbr###########################
library(genderBR)
need_gender <- authors %>% filter(is.na(gender) & First != "") %>% pull(First) %>% unique() 
print(paste(length(need_gender),"need gender, now get from genderBR"))
getgender<-data.frame(First=need_gender,gender = get_gender(need_gender))
if (drop_na(getgender) %>% nrow() > 0){
  authors <- full_join(authors,getgender %>% drop_na(),by="First") %>%
    rowwise() %>%
    mutate(gender = coalesce(gender.x,gender.y)) %>%
    select(-contains(".x") & -contains(".y") & -contains("proportion"))
}

#######################chinesenames###################
library(ChineseNames)
getgender <- givenname %>% select(pinyin,n.male,n.female) %>%
  mutate(First = str_to_title(gsub("[0-9]","",pinyin))) %>%
  group_by(First) %>%
  summarise(gender = if_else(sum(n.male)>sum(n.female), 'male','female'))
authors <- full_join(authors,getgender %>% drop_na(),by="First") %>%
  rowwise() %>%
  mutate(gender = coalesce(gender.x,gender.y)) %>%
  select(-contains(".x") & -contains(".y") & -contains("proportion"))

#######################gender-guesser###########################
library(reticulate)
#conda_create('mac_gender')
use_condaenv('mac_gender')
#py_install('gender-guesser',pip = T)

py_gender <- import("gender_guesser.detector")
gender_detector <- py_gender$Detector()
gender_detector$get_gender("John")

need_gender <- authors %>% filter(is.na(gender) & First != "") %>% pull(First) %>% unique() 
print(paste(length(need_gender),"need gender, now get from gender-guesser"))
getgender<-data.frame(First=need_gender) %>%
  rowwise() %>%
  mutate(gender = gender_detector$get_gender(First),
         gender = case_when(gender %in% c('female','mostly_female') ~ 'female',
                            gender %in% c('male','mostly_male') ~ 'male'))
if (drop_na(getgender) %>% nrow() > 0){
  authors <- full_join(authors,getgender %>% drop_na(),by="First") %>%
    rowwise() %>%
    mutate(gender = coalesce(gender.x,gender.y)) %>%
    select(-contains(".x") & -contains(".y") & -contains("proportion"))
}

######################genderize###################################
library(gender)
need_gender <- authors %>% filter(is.na(gender) & First != "") %>% pull(First) %>% unique() 
print(paste(length(need_gender),"need gender, now get from genderize"))
getgender<-gender(need_gender[sample(length(need_gender),999)],method = "genderize")
if (drop_na(getgender) %>% nrow() > 0){
  authors <- full_join(authors,getgender %>% rename(First = name),by="First") %>%
    rowwise() %>%
    mutate(gender = coalesce(gender.x,gender.y)) %>%
    select(-contains(".x") & -contains(".y") & -contains("proportion"))
}

########################gender API##############################
need_gender <- authors %>% filter(is.na(gender) & First != "") %>% pull(First) %>% unique() 
print(paste(length(need_gender),"need gender, now getting form gender API"))
gender_API_key <- "gender_API_key"
url <- paste0("https://gender-api.com/get?name=",paste(need_gender[sample(length(need_gender),100)],collapse = ";"),"&multi=true&key=",gender_API_key)
getgender <- httr::GET(url) %>% httr::content()
getgender <- do.call(rbind,lapply(getgender$result, function(XX) data.frame(XX))) %>% 
  filter(gender != "unknown") %>% 
  rename(First = name) %>%
  select(First,gender,accuracy)
if (nrow(getgender) > 0) {
  authors <- full_join(authors,getgender ,by="First") %>%
    mutate(gender = coalesce(gender.x,gender.y),
           accuracy = coalesce(accuracy.x,accuracy.y)) %>%
    select(-contains(".x") & -contains(".y"))
}



####################namsor API###########################
library(httr)
library(jsonlite)
need_gender <- authors %>% filter(is.na(gender) & First != "") %>% pull(First) %>% unique() 
print(paste(length(need_gender),"need gender, now getting form Namsor API"))
namsor_API_key <- "namsor_API_key"
url <- "https://v2.namsor.com/NamSorAPIv2/api2/json/genderBatch"
headers <- c(
  "X-API-KEY" = namsor_API_key,
  "Accept" = "application/json",
  "Content-Type" = "application/json"
)

body <- list(personalNames = lapply(need_gender, function(name) list(firstName = name)))
response <- httr::POST(url, add_headers(.headers=headers), body = toJSON(body, auto_unbox = TRUE))
namsor_gender <- fromJSON(content(response, "text"))$personalNames %>% 
  data.frame()
writexl::write_xlsx(namsor_gender,"namsor_gender.xlsx")
getgender <- fromJSON(content(response, "text"))$personalNames %>% 
  data.frame() %>% 
  select(firstName,likelyGender,probabilityCalibrated) %>%
  rename(First = firstName,gender=likelyGender,accuracy = probabilityCalibrated)
if (nrow(getgender) > 0) {
  authors <- full_join(authors,getgender ,by="First") %>%
    mutate(gender = coalesce(gender.x,gender.y),
           accuracy = coalesce(accuracy.x,accuracy.y)) %>%
    select(-contains(".x") & -contains(".y"))
}







###################################finishing#################
authors <- authors %>% mutate(gender = tolower(gender),
                              gender = if_else(gender =='either',NA,gender))

writexl::write_xlsx(authors,"data/authors.xlsx")

authors %>% filter(is.na(gender) & First != "") %>% pull(First) %>% unique()  %>% length()

authors %>% ungroup() %>% filter(!is.na(gender) & First != "") %>%
  select(First,gender,accuracy) %>% unique() %>%
  rbind(gendered) %>% unique() %>%
  distinct(First,.keep_all = T) %>%
  writexl::write_xlsx('data/gendered.xlsx')


