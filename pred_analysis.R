
library(tidyverse)
library(future)
library(sjlabelled)
library(corpus)
library(REDCapR) # Load the package into the current R session.

plan(multisession)

data_refresh = F

data_dict <- read_csv("raw_data/EmergencyDepartmentFormsV2_DataDictionary_2021-07-14.csv")

headache_strings <- c("headache", "migraine", "head ache", 
                      "head pain", "\\bha\\b")
headache_regex <- regex(paste0("(", headache_strings, ")", 
                               collapse="|"), ignore_case = T)

presents_strings <- c("presents(\\swith)?", "presented(\\swith)?", "presenting(\\swith)?",
                      "present(\\swith)?", "pw", "p/w", "here with",
                      "transferred", "transfers", "transfer", 
                      "refers", "referred", "refer", "sent", 
                      "admitted", "admits", "admit", "comes", 
                      "come", "coming", "setting of", 
                      "complaining", "complains", "complain")
presents_regex <- regex(paste0("(", presents_strings, ")", 
                               collapse="|"), ignore_case = T)

if (data_refresh){
  source("raw_data/redcap_info.R")
  
  raw2 <- redcap_read(redcap_uri = uri,
                      token =api_token,
                      batch_size = 1000,
                      interbatch_delay = 1,
                      continue_on_error = TRUE)
  
  df2 <- tibble(`Variable / Field Name` = colnames(raw2$data)) %>%
    left_join(data_dict %>% 
                select(`Variable / Field Name`, `Field Label`),
              by = "Variable / Field Name") %>%
    mutate(`Field Label` = ifelse(
      is.na(`Field Label`), "", `Field Label`)
    ) %>%
    pull(`Field Label`) %>%
    set_label(x = raw2$data, label = .)
  
  
  table_names <- paste0("data_", unique(data_dict$`Form Name`)) %>%
    set_names() %>%
    map(., \(x){ 
      temp <- df2 %>%
        select("record_id", any_of(
          data_dict %>% 
            filter(`Form Name` == str_remove(x, "^data_")) %>%
            pull(`Variable / Field Name`)
        )) %>%
        filter(if_any(.cols = -record_id, .fns = ~ !is.na(.)))
      return(temp)
    })
  
  list2env(table_names, envir = .GlobalEnv)
  
  save(list = names(table_names), file = "raw_data/redcap_data.RData")
  rm(raw2, df2)
  
} else load("raw_data/redcap_data.RData")



data_ed_admission_ha_char <- data_ed_admission_ha_char %>%
  mutate(one_liner = str_sub(entire_note, end = 1000),
         one_liner = str_remove(one_liner, regex("^.*(History of Present Illness\\:(\\s)+[·°º])")),
         one_liner = str_remove(one_liner, regex("^.{0,30}Source\\:.{0,30}[·°º]")),
         one_liner = str_remove(one_liner, regex("^.*CC.{0,5}\\:")),
         one_liner = str_remove(one_liner, regex("\\?")),
         one_liner = str_remove(one_liner, regex("(Time course.*$)|(Symptom onset.*$)")),
         one_liner_sentences = map(one_liner, ~as.character(text_split(.x, units = "sentences")$text)),
         one_liner = str_trim(unlist(
           map(one_liner, 
               \(x){
                 y = unlist(as.character(text_split(x, units = "sentences")$text))
                 y = ifelse(is.na(y), "", y)
                 y_starts = if (any(str_detect(y, regex("^(\\s)+(is a\\b)")))) {
                   first(str_which(y, "\\bis a\\b")) - 1
                 }else if (any(str_detect(y, "\\bis a\\b"))) {
                   first(str_which(y, "\\bis a\\b"))
                 } else 1
                 
                 y_presents = if (any(str_detect(y, presents_regex))){
                   first(str_which(y, presents_regex))
                 }else y_starts+1
                 
                 z = ifelse(
                   str_length(y[y_starts]) > 15 | 
                     is.na(y[2]) | 
                     str_detect(y[y_starts], "\\?") | 
                     !str_detect(y[y_starts], presents_regex), 
                   y[y_starts], paste(y[y_starts:y_presents]))
                 return(z)}))),
         one_liner = ifelse(str_length(one_liner) > 15, one_liner, hist_ed_oth),
         one_liner = str_remove_all(one_liner, regex("[·°º]|(^CC(\\:)?)|(HPI(\\:)?$)")),
         presents_with = str_trim(case_when(
           str_detect(one_liner, presents_regex) ~ str_remove(one_liner, regex(
             paste0("^.*(", presents_strings, ")", collapse="|"), ignore_case = T)),
           str_length(one_liner) < 25 ~ one_liner,
           str_detect(one_liner, regex("\\bwith\\b|\\bw\\b|\\bw\\\b")) ~ str_remove(
             one_liner, regex("^.*(\\bwith\\b|\\bw\\b|\\bw\\\b)(?!.*\\b\\1\\b)")))),
         ha_in_oneliner = str_detect(
           presents_with, 
           regex(paste0("(", headache_strings, ")", 
                        collapse="|"), ignore_case = T)),
         ha_in_cc = str_detect(
           chief_comp, 
           regex(paste0("(", headache_strings, ")", 
                        collapse="|"), ignore_case = T)))


plan(sequential)

data_ed_admission_ha_char %>%
  filter(ha_in_oneliner) %>% 
  group_by(record_id) %>%
  filter(row_number() == 1) %>%
  select(record_id, entire_note, chief_comp, one_liner, presents_with) %>%
  View()

