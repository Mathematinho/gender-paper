library(tidyverse)

meta <- bind_rows(read.csv("data/SCIE.data2022.csv", skip = 2, strip.white = T, na.strings = "N/A") %>%
                    mutate(source='SCIE'),
                  read.csv("data/ESCI.data2022.csv", skip = 2, strip.white = T, na.strings = "N/A") %>%
                    mutate(X2022.JIF = as.numeric(X2022.JIF),
                           source='ESCI')) %>%
  mutate(query = ifelse(is.na(ISSN) | is.na(eISSN),
                        coalesce(ISSN, eISSN),
                        paste(ISSN, "OR", eISSN)),
         query = paste("ISSN (",query,")","AND",'PUBYEAR = 2022')) %>%
  drop_na(X2022.JCI)

writexl::write_xlsx(meta,'data/meta.xlsx')