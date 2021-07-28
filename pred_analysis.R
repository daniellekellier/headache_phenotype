

library(tidyverse)
library(lubridate)
library(future)
library(sjlabelled)
library(corpus)
library(REDCapR)
library(caret)
library(glmnet)
library(ROCR)

plan(multisession) # Data processing is intense. Spread computations over multiple cores

data_refresh = F # Dataset is large and only needs updating sometimes. Check T here to pull and process new data from RedCap

filter_to_first <- "migraine" # can set to "chronic_ha" to include other HA types

# Load in data dictionary to add variable labels to RedCap data
data_dict_loc <- "raw_data/EmergencyDepartmentFormsV2_DataDictionary_2021-07-14.csv"
data_dict <- read_csv(data_dict_loc) %>% # Read in CSV
  mutate(`Choices, Calculations, OR Slider Labels` =  map2( #If a checkbox, make sure to capture all checkbox variables
    `Choices, Calculations, OR Slider Labels`, 
    `Field Type`, ~unlist(ifelse(.y == "checkbox", str_split(.x, "(\\s+)?\\|(\\s+)?"), .x)))) %>%
  unnest(`Choices, Calculations, OR Slider Labels`) %>%
  mutate(`Choices, Calculations, OR Slider Labels` = str_trim(`Choices, Calculations, OR Slider Labels`),
         `Variable / Field Name` = ifelse(
           `Field Type` == "checkbox", glue::glue("{`Variable / Field Name`}___{gsub(',.*$', '', `Choices, Calculations, OR Slider Labels`)}"), 
           `Variable / Field Name`
           ))

headache_strings <- c("headache", "migraine", "head ache", #A list of potential HA words in chief complaint
                      "head pain", "\\bha\\b", "\\bh/a\\b") # Make sure to capture "ha" and "h/a" in note as well
headache_regex <- regex(paste0("(", headache_strings, ")", # Create one long list of strings for regex search
                               collapse="|"), ignore_case = T)

presents_strings <- c("presents(\\swith)?", "presented(\\swith)?", "presenting(\\swith)?", # A list of potential words to signify a "presenting" complaint
                      "present(\\swith)?", "pw", "p/w", "here with", # Be specific to capture different verb tenses + conjugations
                      "transferred", "transfers", "transfer", 
                      "refers", "referred", "refer", "sent", 
                      "admitted", "admits", "admit", "comes", 
                      "come", "coming", "setting of", 
                      "complaining", "complains", "complain")
presents_regex <- regex(paste0("(", presents_strings, ")", # Create one long list of strings for regex search
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



data_ed_admission_ha_char <-  data_ed_admission_ha_char %>%
  mutate(
    entire_note = str_replace_all(
      entire_note, c(
        "\\:(\\s+)?[·°º]{2,}"= "\\: ",
        "^.{0,60}(History of Present Illness\\:(\\s)+)" = "", 
        "[^[:alnum:]]+\\?" = "",
        "(?=\\([^\\)])\\?" = "",
        "(?<!\\([^\\)])elevated troponin?" = "(elevated troponin)",
        "Dr\\." = "Dr",
        "Mr\\." = "Mr",
        "Mrs\\." = "Mrs",
        "Ms\\." = "Ms",
        "\\.(?=.*is a)" = "",
        "St\\." = "St",
        "E\\.(\\s+)?coli" = "E coli")),
    entire_note_sentences = map(
      entire_note, ~unlist(str_split(
        .x, regex("(?<!\\:)(\\s+)?[·°º]{2,}(\\s+)?", 
                  ignore_case = TRUE)))),
    one_liner = str_trim(
      map_chr(entire_note_sentences, 
          \(x){
            
            y = str_subset(
              str_subset(x, regex("(CC\\:)|(\\bis a\\b)|(presents)", ignore_case = T)),
              "Source", negate = TRUE)
            
            z = if (length(y) > 1) {
              if (str_detect(y[[1]], 
                             regex("(CC\\:)|(\\bis a\\b)", ignore_case = T)) &
                  str_detect(y[[2]], 
                             regex("(presents)", ignore_case = T))) {
                str_c(y[1:2], collapse = " ")
                
              } else y[[1]]
            } else if (length(y) == 1) {
              
              y[[1]]
              
            } else str_c(x[[1]], collapse = " ")
            
            return(z)})),
    presents_with = 
      map_chr(one_liner, 
          \(x){
            y = unlist(str_split(x, boundary("sentence")))
            z = str_trim(
              case_when(
              any(str_detect(y, presents_regex)) ~ first(na.omit(str_extract(y, regex(
                paste0("(", presents_strings, ".*$)", collapse="|"), 
                ignore_case = T)))),
              str_length(first(y)) < 25 ~ first(y),
              str_detect(first(y), regex("\\bwith\\b|\\bw\\b|\\bw\\\b")) ~ str_remove(
                first(y), regex("^.*(\\bwith\\b|\\bw\\b|\\bw\\\b)(?!.*\\b\\1\\b)"))))
            
            return(z)}),
         ha_in_oneliner = str_detect(
           presents_with, 
           regex(paste0("(", headache_strings, ")", 
                        collapse="|"), ignore_case = T)),
         ha_in_cc = str_detect(
           chief_comp, 
           regex(paste0("(", headache_strings, ")", 
                        collapse="|"), ignore_case = T)))


ha_diagnoses_first <- ha_diagnoses %>%
  filter(.data[[filter_to_first]]) %>% # Filter to first MIGRAINE diagnosis
  group_by(mrn) %>%
  mutate(across(migraine:other_ha, ~ifelse(.x, as.character(dxdt), NA))) %>%
  summarise(across(migraine:other_ha, ~{ymd(first(na.omit(.x)))}),
            first_dx = first(dxdt)) 
 
data_demographics <- data_demographics %>%
  left_join(ha_diagnoses_first, by = "mrn")
  
data_diagnosis <- data_diagnosis %>%
  mutate(past_cond = as.numeric(rowSums(across(starts_with("past"), 
    ~replace_na(.x, 0))) > 0))

rowsum_columns <- \(x){
  if (all(is.na(c_across(all_of(x))))){
    y = NA_real_
  } else {
    y = as.numeric(sum(c_across(all_of(x)), na.rm = TRUE) > 0)
  }
  return(y)
}


data_ed_first_visits <- data_ed_admission_ha_char %>%
  filter(ha_in_oneliner) %>% 
  group_by(record_id) %>%
  filter(row_number() == 1) %>% ungroup() %>%
  left_join(data_demographics %>% 
              select(record_id, first_dx, sex, dob, race, ethnicity), 
            by = "record_id") %>%
  mutate(dx_before = case_when(
    contact_date >= first_dx ~ 1,
    contact_date <= first_dx ~ 0,
    is.na(first_dx) ~ 0
  )) %>%
  filter(dx_before == 0) %>%
  select(-dx_before) %>%
  left_join(data_diagnosis %>% 
              select(record_id, csn_id_adm = csn_dx, past_cond),
            by = c("record_id", "csn_id_adm")) %>%
  mutate(age = as.numeric(as.duration(
    interval(dob, contact_date)), "years"),
    sex = factor(sex, levels = 1:2, labels = c("Female", "Male")),
    race = factor(race, levels = c(1:8,99), labels = c(
      "American Indian or Alaska Native", "Asian", "Black",
      "Native Hawaiian or Other Pacific Islander", "White", 
      "Multiracial", "Other", "Other", "Other")),
    ethnicity = case_when(
      ethnicity == 1 ~ 1,
      ethnicity == 2 ~ 0,
      TRUE ~ NA_real_),
    dx_within_twelve = case_when(
      first_dx == contact_date ~ 1, 
        first_dx %within% interval(
          contact_date, contact_date+years(1)) ~ 1,
      TRUE ~ 0),
    woke_from = map_dbl(entire_note_sentences, \(x){
      
      y =  str_subset(
        unlist(str_split(x, boundary("sentence"))),
        regex("((\\bwake)|(\\bwaking)|(\\bwoke)|(\\bawake)|(\\bawoke)).{0,40}((from sleep)|((complain|(due to)|from).{0,10}(head|ha\\b|h/a\\b|migraine|pain)))", 
              ignore_case = TRUE))
      
      z = case_when(
        any(str_detect(y, regex("((denie)|(deny)|(not)|(didnt)|(didn\\'t)).{0,20}((\\bwake)|(\\bwaking)|(\\bwoke)|(\\bawake)|(\\bawoke)).{0,40}((from sleep)|((complain|(due to)|from).{0,10}(head|ha\\b|h/a\\b|migraine|pain)))",
                                ignore_case = TRUE))) ~ 0,
        any(str_detect(y, regex("((\\bwake)|(\\bwaking)|(\\bwoke)|(\\bawake)|(\\bawoke)).{0,40}((from sleep)|((complain|(due to)|from).{0,10}(head|ha\\b|h/a\\b|migraine|pain)))",
                                ignore_case = TRUE))) ~ 1,
        TRUE ~ NA_real_
      )
      
      return(z)
    })) %>%
  rowwise() %>%
  mutate(
    nausea_vomit = rowsum_columns(
      c("assoc_sx___nausea", "assoc_sx___vomit", 
        "gi_prob___vomit")),
    photo_phono = rowsum_columns(
      c("assoc_sx___photo", "assoc_sx___noise")),
    fever_total = case_when(
      rowsum_columns(
        c("fever", "overall_prob___fever")) == 1 ~ 1,
      str_detect(fever_oth, "yes|chill") ~ 1,
      is.na(fever_oth) & is.na(fever) & is.na(overall_prob___fever) ~ NA_real_,
      TRUE ~ 0),
    numb_sensory = rowsum_columns(
      c("assoc_sx___numb", "assoc_sx___sensory",
        "neuro_prob___numb")),
    dizzy = case_when(
      heart_prob___dizzy == 1 ~ 1, 
        str_detect(assoc_sx_oth, 
                   regex("(dizzy)|(dizziness)")) ~ 1,
      is.na(heart_prob___dizzy) & is.na(assoc_sx_oth) ~ NA_real_,
      TRUE ~ 0
        ),
    awaken = case_when(
      awaken == 1 ~ 1,
      awaken == 0 ~ 0,
      str_detect(awaken_oth, 
                 regex("(yes)|(\\by\\b)", 
                       ignore_case = T)) ~ 1,
      str_detect(awaken_oth, 
                 "but today HA") ~ 1,
      str_detect(awaken_oth, 
                 regex("(\\bno\\b)|(never)|(\\bn\\b)", 
                       ignore_case = T)) ~ 0,
      woke_from == 1 ~ 1,
      woke_from == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    past_cond = ifelse(is.na(past_cond), 
                       0, past_cond),
    off_hours = as.numeric(
      !between(contact_time, 
               hm("07:00"), hm("19:00")))) %>%
  ungroup()

plan(sequential)



data_ed_first_visits_complete <- data_ed_first_visits %>%
  filter(between(age, 3, 17)) %>%
  mutate(train = as.numeric(contact_date < as.Date("2019-01-01")),
         age = scale(age),
         severity_oth = ifelse(
           severity %in% c(999, 9999) & severity_oth == 'Pain severity: "',
                               map_chr(entire_note_sentences, ~str_c(
                                 str_subset(.x, regex("pain severity", ignore_case = T)), collapse = "; ")),
                               severity_oth),
         side = relevel(fct_collapse(
           side, `bilateral` = c("bi"),
                                     `unilateral` = c("right", "left", "uni_l_r"),
                                     `other` = c("oth")), ref = "unilateral"),
         severity_new = case_when(
           severity %in% 7:10 ~ 1,
           severity %in% 0:6 ~ 0,
           severity %in% c(999, 9999) & str_detect(entire_note, regex("(7|8|9|(10))(\\s+)?(\\/|(out)? of)(\\s+)?10", ignore_case = T)) ~ 1,
           severity %in% c(999, 9999) & str_detect(severity_oth, regex("((?<!(most|more).{0,10})severe)|devere|excruciating|\\b(7|8|9|(?<![0-9/].{0,3})10)\\b(?!.{0,10}(hour|\\bmins|minute|day))|(very bad)", ignore_case = T)) ~ 1,
           severity %in% c(999, 9999) & str_detect(severity_oth, regex("mild|moderate", ignore_case = T)) ~ 0,
           severity %in% c(999, 9999) & str_detect(entire_note, regex("([0-6](\\s+)?(\\/|(out)? of)(\\s+)?10)|(\\b[0-6]\\b(?!.{0,10}(hour|\\bmins|minute|day)))", ignore_case = T)) ~ 0
           ))  %>%
  select(record_id, dx_within_twelve, train, contact_date, age, sex, past_cond,
         occipital = location___occ, 
         fever_total, nausea_vomit, 
         photophobia = assoc_sx___photo, phonophobia = assoc_sx___noise,
         vision_changes = assoc_sx___vision, dizzy,
         worsen_activity = assoc_sx___active,
         altered_mental = neuro_prob___alt_mental,
         side, severity_new) %>%
  filter(complete.cases(.)) %>%
  filter(contact_date < as.Date("2020-03-01")) 

p1 <- data_ed_first_visits_complete %>%
  mutate(year = year(contact_date)) %>%
ggplot(data = ., aes(year, fill = factor(train))) +
  geom_bar(stat = "count") +
  labs(x = "Year", y = "Count") +
  ggthemes::scale_fill_ptol() +
  ggthemes::theme_tufte(base_size = 20, base_family = "sans") +
  theme(legend.position = "none")

data_ed_first_visits_train <- data_ed_first_visits_complete %>%
  filter(train == 1) %>%
  select(-record_id, -train, -contact_date) %>%
  slice_sample(n = nrow(.), replace = TRUE)

data_ed_first_visits_test <- data_ed_first_visits_complete %>%
  filter(train == 0) %>%
  select(-record_id, -train, -contact_date)


set.seed(42)
cv_10 = trainControl(method = "cv", number = 10)

ed_elnet = train(
  factor(dx_within_twelve) ~ ., 
  data = data_ed_first_visits_train,
  method = "glmnet",
  trControl = cv_10,
  tuneLength = 10
)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}


encoder <- onehot::onehot(data_ed_first_visits_train)
data_ed_first_visits_train_encode <- predict(encoder, data_ed_first_visits_train)
data_ed_first_visits_test_encode <- predict(encoder, data_ed_first_visits_test)

ed_elnet2 <- cv.glmnet(x = data_ed_first_visits_train_encode[,-1],
          y = data_ed_first_visits_train_encode[,1],
          family = "binomial",
          nfolds = 10,
          relax = TRUE,
          gamma = seq(0,1,0.1))

plot(ed_elnet2, label = T, xvar = 'lambda')


elnet_train_preds <- as.vector(
  predict(ed_elnet2,
          type = "response", s = "lambda.1se", gamma = "gamma.1se",
          as.matrix(data_ed_first_visits_train_encode[,-1])
  ))

coef(ed_elnet2, s = "lambda.1se", gamma = "gamma.1se",)
  
elnet_test_preds <- as.vector(
  predict(ed_elnet2,
          type = "response", s = "lambda.1se",  gamma = "gamma.1se",
          as.matrix(data_ed_first_visits_test_encode[,-1])
  ))

elnet_prediction <- prediction(elnet_test_preds,
                               data_ed_first_visits_test_encode[,1])

plot(performance(elnet_prediction, 'tpr', 'fpr'), col = 'green')
lines(c(0,1),c(0,1),col = "gray", lty = 4 )

performance(elnet_prediction, 'auc')@y.values[[1]]

tibble(y =  factor(data_ed_first_visits_test_encode[,1]),
         mod = elnet_test_preds) %>%
  calibration(y ~ mod, data = ., cuts = 10) %>%
  ggplot(.) + 
  ggthemes::theme_tufte(base_size = 20, base_family = "sans")

pROC::auc(factor(data_ed_first_visits_test_encode[,1]), 
          elnet_test_preds)
pROC::ci.auc(factor(data_ed_first_visits_test_encode[,1]), 
             elnet_test_preds, conf.level = 0.95,
             boot.stratified = T,
             method = "bootstrap") 

x <- seq(0.05,0.95, 0.05) %>%
  map_df(., \(x){
    threshold = x
    
    y = tibble(threshold = threshold,
               obs =  data_ed_first_visits_test_encode[,1],
           pred_prob = elnet_test_preds,
           pred_resp = as.numeric(pred_prob >= threshold),
           correct = as.numeric(obs == pred_resp))
    
    ppv = y %>%
      group_by(threshold, pred_resp) %>%
      summarise(prop_correct = mean(correct)) %>%
      mutate(pred_resp = factor(pred_resp, 
                                levels = c(0,1),
                                labels = c("NPV", "PPV"))) %>%
      pivot_wider(names_from = pred_resp, values_from = prop_correct) 

    
    return(ppv)
  })

confusionMatrix(
  data = as.factor(elnet_test_preds >= 0.2), 
  reference = factor(data_ed_first_visits_test_encode[,1] == 1),
  positive = "TRUE"
  )

test_demographics <- data_ed_first_visits_complete %>%
  filter(train == 0) %>%
  left_join(data_ed_first_visits %>% 
              select(record_id, race, ethnicity),
            by = "record_id") %>%
  mutate(
    pred_probs = elnet_test_preds,
    ethnicity = factor(
      ethnicity, levels = 0:1, 
      labels = c("Non-Hispanic/Latinx", "Hispanic/Latinx")),
    race = fct_lump_n(race, n = 3)) %>%
  select(record_id, dx_within_twelve, sex, race, 
         ethnicity, pred_probs) 

test_demographics %>%
  pivot_longer(cols = c(dx_within_twelve, pred_probs)) %>%
  mutate(name = case_when(
    name == "dx_within_twelve" ~ "Observed",
    name == "pred_probs" ~ "Predicted"
  )) %>%
  group_by(race, name) %>%
  summarise(mean_se(value, mult = 1.96),
            .groups = "drop") %>%
  ggplot(., aes(x = race, y = y, ymin = ymin, ymax = ymax,
                fill = name, group = name)) +
  geom_col(width = 0.5, position = "dodge") +
  geom_errorbar(width = 0.2, position = position_dodge(0.5)) +
  labs(x = "Race", y = "Probability of Migraine Dx",
       fill = "") +
  ggthemes::scale_fill_ptol() +
  ggthemes::theme_tufte(base_size = 20, base_family = "sans")


test_demographics %>%
  group_by(race) %>%
  summarise(
    dx_within_twelve = mean(dx_within_twelve, na.rm = T),
    n = n(),
    .groups = "drop") %>%
  mutate(dx_within_twelve = ifelse(
    dx_within_twelve == 0,
    0.005, dx_within_twelve
  )) %>%
  ggplot(., aes(x = race, y = dx_within_twelve)) +
  geom_col(width = 0.5, position = "dodge") +
  scale_y_continuous(limits = c(-0.02,0.3), 
                     breaks = seq(0, 0.3, 0.05)) +
  labs(x = "Race", y = "Predicted Probability") +
  ggthemes::scale_fill_ptol() +
  ggthemes::theme_tufte(base_size = 20, base_family = "sans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


