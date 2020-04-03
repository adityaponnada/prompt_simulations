## Import libraries
library(psych)
library(MASS)
library(dplyr)
library(ggplot2)
library(rjson)
library(reshape2)

## read the questions json file

question_list <- fromJSON(file="questions.json")

### don't convert to df. Does not help
question_df <- as.data.frame(question_list)

### Wake time assumption
study_dur = 100
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
final_prompt_list = list()
for (i in 1:study_dur){
  print(paste0("For day: ", i))
  day_level_list = list()
  for (j in 1:total_prompts_day){
    ## Generate a random number between 1 - total questions
    rnd_index = sample(1:total_questions, 1)
    day_level_list[j] <- question_list[[rnd_index]]['id']
  }
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
}

length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))

random_df <- as.data.frame(table(full_prompted_list))

ggplot(random_df, aes(x=full_prompted_list, y=Freq)) + geom_bar(stat="identity") + 
  labs(title = "Random selection", x = "\nQuestions", y="\nFrequency") +
  theme(axis.text.x = element_text(angle=70, hjust=1))
        