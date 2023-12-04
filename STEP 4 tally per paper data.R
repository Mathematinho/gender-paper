library(tidyverse)
library(readxl)

authors <- read_xlsx('data/authors.xlsx') %>% 
  filter(!is.na(JCR.Abbreviation)) %>% 
  distinct(JCR.Abbreviation,entry_number,`@seq`,`afid.$`,rank, gender)

by_paper1 <- authors %>% group_by(JCR.Abbreviation,entry_number) %>% 
  drop_na(gender) %>%
  summarise(total = n(),
            female = sum(gender=='female',na.rm=T)) 

by_paper2 <- authors %>%  group_by(JCR.Abbreviation,entry_number) %>% 
  drop_na(rank) %>% drop_na(gender) %>% select(rank, gender) %>% 
  pivot_wider(names_from = rank,values_from = gender)

by_paper <- full_join(by_paper1,by_paper2) 

papers <- read_xlsx('data/papers.xlsx')
meta <- read_xlsx('data/meta.xlsx') %>%
  mutate(qt = cut(X2022.JIF, quantile(X2022.JIF, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), include.lowest = TRUE, labels = c("Q4", "Q3", "Q2", "Q1")))

table(meta$source,meta$qt)


meta %>% group_by(qt,source) %>% summarize(jif = mean(X2022.JIF))


alldata <- full_join(papers,by_paper) %>% full_join(meta %>% select(JCR.Abbreviation,X2022.JIF,JIF.Quartile,qt,source))

#subtypes
table(alldata$subtype,useNA = 'always')

alldata_final <- alldata %>% filter(subtype %in% c("Article","Editorial","Note","Review"))

#subtypes
table(alldata_final$subtype,useNA = 'always')
#total authors 
alldata_final$ttotal %>% as.numeric() %>% sum(na.rm=T)
#total authors gender available
alldata_final$total %>% sum(na.rm=T)
#total paper at least one first/seinor gender available
alldata_final %>% filter(!(is.na(`First Author`) | is.na(`Senior Author`))) %>% dim()
#total paper at least one first/seinor gender not available
alldata_final %>% filter(is.na(`First Author`) | is.na(`Senior Author`)) %>% dim()


#total papers
alldata_final %>% dim()

#keep paper with both first and seinor gender
alldata_final <- alldata_final %>% filter(!(is.na(`First Author`) | is.na(`Senior Author`))) %>%
  mutate(other_total = total-2,
         other_female = female - 1*(`First Author` == 'female') - 1*(`Senior Author` == 'female'))


#total papers
alldata_final %>% dim()

#all types
alldata_final$subtype %>% table()
alldata_final$source %>% table()





alldata_final %>% writexl::write_xlsx("data/alldata_final.xlsx")
alldata_long <- alldata_final %>% pivot_longer(cols = c(`First Author`,`Senior Author`),
                                               names_to = "rank",
                                               values_to = 'gender')
alldata_long %>% writexl::write_xlsx('data/alldata_long.xlsx')
