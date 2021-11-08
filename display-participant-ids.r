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


#### gorkem's approach ####

#### X- SONA chits awards ####
firstTimeSlot <- read.csv("sonaFirstTimeSlot.csv", header= TRUE)
partPeopleHere <- unique(propRightAndDown.All$subNum)
#write.csv(x=unique(propRightAndDown.All$subNum), file="participatedPeople.csv")

#check each participant ID and see if there is a data of that here. 
for (i in 1:nrow(firstTimeSlot)){
  partIDonSONA <- firstTimeSlot$survey_id[i]
  if (is.element(partIDonSONA, partPeopleHere)){
    firstTimeSlot$dataYesOnMYSQL[i] <- 1 
  }
  else {
    firstTimeSlot$dataYesOnMYSQL[i] <- 0 
  }
  if (firstTimeSlot$dataYesOnMYSQL[i] == 1 && firstTimeSlot$credit_type[i] =="Participated"){
    firstTimeSlot$awardThisPerson[i] <- "awarded"
  }
  else if (firstTimeSlot$dataYesOnMYSQL[i] == 1 && firstTimeSlot$credit_type[i] =="Awaiting Action"){
    firstTimeSlot$awardThisPerson[i] <- "should be awarded"
  }
  else if (firstTimeSlot$dataYesOnMYSQL[i] == 0 && firstTimeSlot$credit_type[i] =="Participated"){
    firstTimeSlot$awardThisPerson[i] <- "should NOT be awarded"
  }
  else {
    firstTimeSlot$awardThisPerson[i] <- "no"
  }
}


write.csv(firstTimeSlot, file = "sonaFirstTimeSlot_processed.csv")
#get me the "should be awarded people"
awardThesePeople <- subset (firstTimeSlot, firstTimeSlot$awardThisPerson == "should be awarded")
awardThesePeople$survey_id

awardedButShouldntBe <- subset (firstTimeSlot, firstTimeSlot$awardThisPerson == "should NOT be awarded")
awardedButShouldntBe$survey_id

noShowPeep <- subset (firstTimeSlot, firstTimeSlot$awardThisPerson == "no")
noShowPeep$survey_id

#### 
