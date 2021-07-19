
library(tidyverse)
library(lubridate)
library(future)
library(sjlabelled)
library(corpus)
library(REDCapR) # Load the package into the current R session.

plan(multisession)

data_refresh = F

data_dict <- read_csv("raw_data/EmergencyDepartmentFormsV2_DataDictionary_2021-07-14.csv") %>%
  mutate(`Choices, Calculations, OR Slider Labels` =  map2(
    `Choices, Calculations, OR Slider Labels`, 
    `Field Type`, ~unlist(ifelse(.y == "checkbox", str_split(.x, "(\\s+)?\\|(\\s+)?"), .x)))) %>%
  unnest(`Choices, Calculations, OR Slider Labels`) %>%
  mutate(`Choices, Calculations, OR Slider Labels` = str_trim(`Choices, Calculations, OR Slider Labels`),
         `Variable / Field Name` = ifelse(
           `Field Type` == "checkbox", glue::glue("{`Variable / Field Name`}___{gsub(',.*$', '', `Choices, Calculations, OR Slider Labels`)}"), 
           `Variable / Field Name`
           ))

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
  
  ha_diagnoses <- readxl::read_xlsx(path = "raw_data/ed_icd_codes.xlsx") %>%
    rename_with(tolower) %>%
    rename(mrn = pat_mrn_id) %>%
    mutate(dxdt = parse_date(dxdt, format = "%d-%b-%y")) %>%
    mutate(migraine = str_detect(icd_code, regex("G43(?!\\.[AD])|346")),
           tth = str_detect(icd_code, regex("G44\\.21|G44\\.22|339\\.11|339\\.12")),
           cluster = str_detect(icd_code, regex("G44\\.01|G44\\.02|339.\\01|339\\.02")),
           other_ha = str_detect(icd_code, regex("G44\\.03|G44\\.04|G44\\.321|G44\\.51|G44\\.52|339\\.03|339\\.04|339\\.22|339\\.41|339\\.42")),
           chronic_ha = rowSums(across(migraine:other_ha, as.numeric)) > 0)
  
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
            filter(
              `Form Name` == str_remove(x, "^data_")) %>%
            pull(`Variable / Field Name`) %>% unique())) %>%
        filter(if_any(.cols = -record_id, .fns = ~ !is.na(.)))
      return(temp)
    })
  
  list2env(table_names, envir = .GlobalEnv)
  
  save(list = c("ha_diagnoses", names(table_names)), file = "raw_data/redcap_data.RData")

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

ha_diagnoses_first <- ha_diagnoses %>%
  filter(chronic_ha) %>%
  group_by(mrn) %>%
  mutate(across(migraine:other_ha, ~ifelse(.x, as.character(dxdt), NA))) %>%
  summarise(across(migraine:other_ha, ~{ymd(first(na.omit(.x)))}),
            first_dx = first(dxdt)) 
 
data_demographics <- data_demographics %>%
  left_join(ha_diagnoses_first, by = "mrn")
  
data_diagnosis <- data_diagnosis %>%
  mutate(past_cond = as.numeric(rowSums(across(starts_with("past"), 
    ~replace_na(.x, 0))) > 0))


data_ed_first_visits <- data_ed_admission_ha_char %>%
  filter(ha_in_oneliner) %>% 
  group_by(record_id) %>%
  filter(row_number() == 1) %>% ungroup() %>%
  left_join(data_demographics %>% 
              select(record_id, first_dx, sex, dob, race, ethnicity), 
            by = "record_id") %>%
  filter(contact_date >= first_dx) %>%
  left_join(data_diagnosis %>% 
              select(record_id, csn_id_adm = csn_dx, past_cond),
            by = c("record_id", "csn_id_adm")) %>%
  mutate(age = as.numeric(as.duration(interval(dob, contact_date)), "years"),
         sex = factor(sex, levels = 1:2, labels = c("Female", "Male")),
         race = factor(race, levels = c(1:8,99), labels = c(
           "American Indian or Alaska Native", "Asian", "Black",
           "Native Hawaiian or Other Pacific Islander", "White", 
           "Multiracial", "Other", "Other", "Other")),
         ethnicity = case_when(
           ethnicity == 1 ~ 1,
           ethnicity == 2 ~ 0,
           TRUE ~ NA_real_),
         dx_within_twelve = (
           first_dx == contact_date | 
             first_dx %within% interval(
               contact_date, contact_date+years(1))),
         nausea_vomit = as.numeric(rowSums(across(
           c("assoc_sx___nausea", "assoc_sx___vomit", 
             "gi_prob___vomit"), 
           as.numeric), na.rm = T) > 0),
         photo_phono = as.numeric(rowSums(across(
           c("assoc_sx___photo", "assoc_sx___noise"), 
           as.numeric), na.rm = T) > 0),
         fever_total = as.numeric(rowSums(across(
           c("fever", "overall_prob___fever"), 
           as.numeric), na.rm = T) > 0 |
             str_detect(fever_oth, "yes|chill")),
         numb_sensory = as.numeric(rowSums(across(
           c("assoc_sx___numb", "assoc_sx___sensory",
             "neuro_prob___numb"), 
           as.numeric), na.rm = T) > 0),
         dizzy = as.numeric(
           heart_prob___dizzy == 1 | 
             str_detect(assoc_sx_oth, regex("(dizzy)|(dizziness)"))),
         awaken = case_when(
           awaken == 1 ~ 1,
           awaken == 0 ~ 0,
           str_detect(awaken_oth, regex("(yes)|(\\by\\b)", ignore_case = T)) ~ 1,
           str_detect(awaken_oth, "but today HA") ~ 1,
           str_detect(awaken_oth, regex("(\\bno\\b)|(never)|(\\bn\\b)", ignore_case = T)) ~ 0,
           TRUE ~ NA_real_
         ),
         fundus_examined = as.numeric(!is.na(fundus)),
         past_cond = ifelse(is.na(past_cond), 0, past_cond),
         off_hours = as.numeric(!between(contact_time, hm("07:00"), hm("19:00"))))

data_ed_first_visits %>%
  select(dx_within_twelve, age, sex, race, ethnicity, past_cond,
         seiz_hist = hist_ed___sz, off_hours, 
         occipital = location___occ, 
         awaken, fever_total, nausea_vomit, photo_phono, 
         vision_changes = assoc_sx___vision, dizzy,
         worsen_activity = assoc_sx___active,
         altered_mental = neuro_prob___alt_mental,
         fundus_examined) %>%
  summarise(across(everything(), ~scales::percent(
    sum(is.na(.x))/n(), accuracy = 0.0001)))


