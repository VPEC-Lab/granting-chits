# assign SONA credit for participants
# that participated in both Qualtrics and
# the dissertation Study 1

library(tidyverse)
library(readr)



# Qualtrics data ----------------------------------------------------------


qualtrics <- read_csv("Study 1: Holistic processing and deception detection demographics - Fall 2021_November 7, 2021_17.12.csv")
View(Study_1_Holistic_processing_and_deception_detection_demographics_Fall_2021_November_2_2021_15_07)

qualtrics_var_names <- names(qualtrics)

qualtrics_data <- read_csv("Study 1: Holistic processing and deception detection demographics - Fall 2021_November 7, 2021_17.12.csv",
                          col_names = qualtrics_var_names, 
                          skip = 3)

qualtrics_ids <- qualtrics_data %>% 
  select(id, EndDate) %>% 
  unique() %>% 
  mutate(source = "qualtrics")

# Dissertation Study 1 data -----------------------------------------------

# 10/31/2021

compfaces <- read_csv('compFaces7Nov.csv')

names(compfaces)


compfaces_ids <- compfaces %>% 
  select(participant_ID) %>% 
  unique() %>% 
  mutate(source = "experiment",
         id = participant_ID)

# Merge Qualtrics and Experiment data -------------------------------------

merged_data <- left_join(qualtrics_ids, compfaces_ids, by=c("id")) %>% 
  mutate(completed_study = case_when(
    id == participant_ID ~ 1
  )) %>% 
  mutate(completed_study = replace_na(completed_study, 0)
  )


View(merged_data)
# Create dataset of IDs of completed participants -------------------------

full_study_id_list <- merged_data %>% 
  filter(completed_study == 1) %>% 
  select(EndDate, participant_ID)

View(full_study_id_list)

# save files --------------------------------------------------------------

write.csv(merged_data, file = 'qualtrics-compfaces-merged-ids.csv')

write.csv(full_study_id_list, file = 'full_study_id_list.csv')

