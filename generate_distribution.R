## Import libraries
library(psych)
library(MASS)
library(dplyr)
library(ggplot2)
library(rjson)
library(reshape2)
library(stringi)
library(rlist)

## read the questions json file

question_list <- fromJSON(file="questions.json")

### don't convert to df. Does not help
question_df <- as.data.frame(question_list)

### Wake time assumption
study_dur = 270
sleep_dur = 6.0
DAY = 24.0
BUFFER = 1.0
wake_dur = DAY - BUFFER - sleep_dur
prompts_per_hour = 4.0

total_prompts_day = as.integer(wake_dur*prompts_per_hour)

total_questions = length(question_list)

## Assuming 10% go to engagement/validation question
total_prompts_day = as.integer(total_prompts_day - (total_prompts_day*0.1))

### Test draw any item at random from the list for the assumed total_prompts a day

### Create an empty total prompt list
final_prompt_list = list()
selected_day_prompts = list()
### run for study duration
for (i in 1:study_dur){
  print(paste0("For day: ", i))
  day_level_list = list()
  for (j in 1:total_prompts_day){
    ## Generate a random number between 1 - total questions
    rnd_index = sample(1:total_questions, 1)
    day_level_list[j] <- question_list[[rnd_index]]['id']
  }
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
  if (i == 1){
    selected_day_prompts = day_level_list 
  }
}

length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))
length(full_prompted_list)

random_df <- as.data.frame(prop.table(table(full_prompted_list)))
random_df$full_prompted_list <- unlist(lapply(strsplit(as.character(random_df$full_prompted_list), "_"), '[[', 1))

## Plot distribution
ggplot(random_df, aes(x=full_prompted_list, y=Freq)) + geom_bar(stat="identity") + 
  labs(title = "Random selection", x = "\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

## Plot for a given day's prompting
length(selected_day_prompts)
selected_day_prompts <- unlist(unlist(selected_day_prompts, recursive = FALSE))
length(unique(selected_day_prompts))
day_df <- as.data.frame(table(selected_day_prompts))
day_df$selected_day_prompts <- unlist(lapply(strsplit(as.character(day_df$selected_day_prompts), "_"), '[[', 1))

ggplot(day_df, aes(x=selected_day_prompts, y=Freq)) + geom_bar(stat = "identity") +
  labs(title="Random selection | Day 1", x="\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x=element_text(angle = 70, hjust = 1))

### Including set types
set_types = c("External", "Internal", "Reflective", "Reactive")

## No. of items Reflective - 7, Reactive - 3, Internal - 11, External - 10
get_questions_day = c(5, 4, 3, 2)

### Get the day-level question list first
external_questions = list.filter(question_list, type=="External")
internal_questions = list.filter(question_list, type=="Internal")
reflective_questions = list.filter(question_list, type=="Reflective")
reactive_questions = list.filter(question_list, type=="Reactive")

### Create an empty total prompt list
final_prompt_list = list()
selected_day_prompts = list()
### run for study duration
for (i in 1:study_dur){
  print(paste0("For day: ", i))
  ext_day = sample(external_questions, get_questions_day[1])
  int_day = sample(internal_questions, get_questions_day[2])
  refl_day = sample(reflective_questions, get_questions_day[3])
  react_day = sample(reactive_questions, get_questions_day[4])
  questions_day = c(ext_day, int_day, refl_day, react_day)
  total_questions = length(questions_day)
  day_level_list = list()
  for (j in 1:total_prompts_day){
    ## Generate a random number between 1 - total questions
    rnd_index = sample(1:total_questions, 1)
    day_level_list[j] <- questions_day[[rnd_index]]['id']
  }
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
  if (i == 1){
    selected_day_prompts = day_level_list 
  }
}

length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))
length(full_prompted_list)
random_df <- as.data.frame(prop.table(table(full_prompted_list)))
random_df$full_prompted_list <- unlist(lapply(strsplit(as.character(random_df$full_prompted_list), "_"), '[[', 1))

## Plot distribution
ggplot(random_df, aes(x=full_prompted_list, y=Freq)) + geom_bar(stat="identity") + 
  labs(title = "Set-based selection", x = "\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

## Plot for a given day's prompting
length(selected_day_prompts)
selected_day_prompts <- unlist(unlist(selected_day_prompts, recursive = FALSE))
length(unique(selected_day_prompts))
day_df <- as.data.frame(table(selected_day_prompts))
day_df$selected_day_prompts <- unlist(lapply(strsplit(as.character(day_df$selected_day_prompts), "_"), '[[', 1))

ggplot(day_df, aes(x=selected_day_prompts, y=Freq)) + geom_bar(stat = "identity") +
  labs(title="Set-based selection | Day 1", x="\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x=element_text(angle = 70, hjust = 1))



## Use maxed out caps to prevent over prompting in a day
### Including set types
set_types = c("External", "Internal", "Reflective", "Reactive")

## No. of items Reflective - 7, Reactive - 3, Internal - 11, External - 10
get_questions_day = c(5, 4, 3, 2)

### Get the day-level question list first
external_questions = list.filter(question_list, type=="External")
internal_questions = list.filter(question_list, type=="Internal")
reflective_questions = list.filter(question_list, type=="Reflective")
reactive_questions = list.filter(question_list, type=="Reactive")

### Create an empty total prompt list
final_prompt_list = list()
selected_day_prompts = list()
### run for study duration
for (i in 1:study_dur){
  print(paste0("For day: ", i))
  ext_day = sample(external_questions, get_questions_day[1])
  int_day = sample(internal_questions, get_questions_day[2])
  refl_day = sample(reflective_questions, get_questions_day[3])
  react_day = sample(reactive_questions, get_questions_day[4])
  
  questions_day = c(ext_day, int_day, refl_day, react_day)
  total_questions = length(questions_day)
  
  ## Add a max check variable to the list
  for (ind in 1:length(questions_day)){
    questions_day[[ind]]["count"] <- 0
  }
  
  day_level_list = list()
  
  for (j in 1:total_prompts_day){
    ## Filter out already maxed out prompts
    # print(paste0("Total questions in list: ", length(questions_day)))
    questions_day <- list.filter(questions_day, count < max_prompts_per_day)
    # print(paste0("In filtered list: ", length(questions_day)))
    
    total_current_questons = length(questions_day)
    
    ## Generate a random number between 1 - total questions
    rnd_index = sample(1:total_current_questons, 1)
    day_level_list[j] <- questions_day[[rnd_index]]['id']
    quest_count = questions_day[[rnd_index]]['count']$count
    questions_day[[rnd_index]]['count']$count = quest_count + 1.0
  }
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
  if (i == 1){
    selected_day_prompts = day_level_list 
  }
}

length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))
length(full_prompted_list)
random_df <- as.data.frame(prop.table(table(full_prompted_list)))
random_df$full_prompted_list <- unlist(lapply(strsplit(as.character(random_df$full_prompted_list), "_"), '[[', 1))

## Plot distribution
ggplot(random_df, aes(x=full_prompted_list, y=Freq)) + geom_bar(stat="identity") + 
  labs(title = "Set-based + max filter selection", x = "\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

## Plot for a given day's prompting
length(selected_day_prompts)
selected_day_prompts <- unlist(unlist(selected_day_prompts, recursive = FALSE))
length(unique(selected_day_prompts))
day_df <- as.data.frame(table(selected_day_prompts))
day_df$selected_day_prompts <- unlist(lapply(strsplit(as.character(day_df$selected_day_prompts), "_"), '[[', 1))

ggplot(day_df, aes(x=selected_day_prompts, y=Freq)) + geom_bar(stat = "identity") +
  labs(title="Set-based + max filter selection | Day 1", x="\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x=element_text(angle = 70, hjust = 1))


