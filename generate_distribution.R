## Import libraries
library(psych)
library(MASS)
library(dplyr)
library(ggplot2)
library(rjson)
library(reshape2)
library(stringi)
library(rlist)
library(plyr)

# The purpose of this script is to simulate uEMA prompting strategies.
# In addiion, we want to compute the expected values of: 
#   1) % of the time the question will be answered throughout the study
#   2) number of days a question will show up throughout the study


## read the questions json file

question_list <- fromJSON(file="questions.json")


### Wake time assumption
study_dur = 10000
sleep_dur = 8.0
DAY = 24.0
BUFFER = 1.0
wake_dur = DAY - BUFFER - sleep_dur
prompts_per_hour = 4.0
completion_rate = 0.75
completion_counter = 0

total_prompts_day = as.integer(wake_dur*prompts_per_hour)

## Filter out the context-triggered questions
question_list = list.filter(question_list, is_context_sensitive == FALSE)

total_questions = length(question_list)

## Assuming 10% go to engagement/validation question
total_prompts_day = as.integer(total_prompts_day - (total_prompts_day*0.1))

### Add a category to each question type

get_type  <- function(x, q_list){
  # id == x
  temp_list <- list.filter(q_list, id == x)
  type_result = temp_list[[1]]$type
  return(type_result)
}

### Get prelim plot of max prompt per day
q_content <- c()
max_prompt_content <- c()
type_content <- c()
for (i in 1:length(question_list)){
  q_content <- c(q_content, as.character(question_list[[i]]['id']$id))
  max_prompt_content <- c(max_prompt_content, question_list[[i]]['max_prompts_per_day']$max_prompts_per_day)
  type_content <- c(type_content, as.character(question_list[[i]]['type']$type))
}

question_df2 <- cbind(q_content, type_content, max_prompt_content)
question_df2 <- as.data.frame(question_df2)
question_df2$max_prompt_content <- as.numeric(levels(question_df2$max_prompt_content))[question_df2$max_prompt_content]
names(question_df2) <- c("QUESTION", "TYPE", "MAX_PROMPT")

# Plot max prompt distriution
ggplot(question_df2, aes(x=QUESTION, y=MAX_PROMPT, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Max prompts allowed", x = "\nQuestions", y="\nMax no. prompts") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

### Test draw any item at random from the list for the assumed total_prompts a day

### Create an empty total prompt list
final_prompt_list = list()
selected_day_prompts = list()
completion_counter_list = list()
question_df_with_day = data.frame(QUESTION = NA, DAY = NA)
### run for study duration
for (i in 1:study_dur){
  print(paste0("For day: ", i))
  completion_counter = 0
  day_level_list = list()
  for (j in 1:total_prompts_day){
    ## Generate a random number between 1 - total questions
    rnd_index = sample(1:total_questions, 1)
    ## add an assumed completion rate to get only the answered prompts in the list
    rnd_completion = runif(1)
    if (rnd_completion <= completion_rate){
      completion_counter = completion_counter + 1
      day_level_list[j] <- question_list[[rnd_index]]['id']
      question_id <- question_list[[rnd_index]]['id']$id
      day_into_study <- i
      question_df_with_day <- rbind(question_df_with_day, c(question_id, day_into_study))
    }
    
  }
  
  completion_counter_list[i] <- completion_counter
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
  if (i == 1){
    selected_day_prompts = day_level_list 
  }
}

## Clear the NA row
question_df_with_day <- question_df_with_day[-1,]

# Add question category type:
for (i in 1:nrow(question_df_with_day)){
  question_df_with_day$TYPE[i] <- get_type(as.character(question_df_with_day$QUESTION[i]), question_list)
}

question_df_with_day$QUESTION <- lapply(strsplit(as.character(question_df_with_day$QUESTION), "_"), '[[', 1)

## Convert to factors
question_df_with_day$QUESTION <- as.factor(unlist(question_df_with_day$QUESTION))
question_df_with_day$DAY <- as.factor(question_df_with_day$DAY)

# Create a condensed mapped data frame for question and type
question_type_map <- unique(question_df_with_day[c("QUESTION", "TYPE")])

get_type_for_df <- function (x){
  question_type = question_type_map$TYPE[question_type_map$QUESTION == x]
  return (question_type)
  
}

## Create a contingency table with question and the day times
question_number_of_days <- aggregate(DAY ~ QUESTION, question_df_with_day, function(x) length(unique(x)))
question_number_of_days$DAY_PERCENT <- question_number_of_days$DAY/study_dur

question_number_of_days$TYPE <- lapply(question_number_of_days$QUESTION, get_type_for_df)
question_number_of_days$TYPE <- as.factor(unlist(question_number_of_days$TYPE))

# Write this to a file before plotting
write.csv(question_number_of_days, file = "D:/uEMA_Simulation_plots/Improved_plots/random_prompting_question_daywise_distribution.csv", sep = ",")

# Plot with number of days
ggplot(question_number_of_days, aes(x=QUESTION, y=DAY, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Random selection | daywise distribution", x = "\nQuestions", y="\nNumber of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

ggplot(question_number_of_days, aes(x=QUESTION, y=DAY_PERCENT, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Random selection | daywise distribution", x = "\nQuestions", y="\n% of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))


length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))
length(full_prompted_list)

random_df <- as.data.frame(prop.table(table(full_prompted_list)))

for (i in 1:nrow(random_df)){
  random_df$TYPE[i] <- get_type(as.character(random_df$full_prompted_list[i]), question_list)
}

random_df$full_prompted_list <- unlist(lapply(strsplit(as.character(random_df$full_prompted_list), "_"), '[[', 1))


## Plot distribution
ggplot(random_df, aes(x=full_prompted_list, y=Freq, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Random selection", x = "\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

# Save the study duration prompt distribution file
write.csv(random_df, file="D:/uEMA_Simulation_plots/Improved_plots/random_prompting.csv", sep = ",")

## Plot for a given day's prompting
length(selected_day_prompts)
selected_day_prompts <- unlist(unlist(selected_day_prompts, recursive = FALSE))
length(unique(selected_day_prompts))
day_df <- as.data.frame(table(selected_day_prompts))

# Save the sequence of plotting for a random day
write(selected_day_prompts, file="D:/uEMA_Simulation_plots/Improved_plots/random_day_list.txt", sep = ",")


for (i in 1:nrow(day_df)){
  day_df$TYPE[i] <- get_type(as.character(day_df$selected_day_prompts[i]), question_list)
}

day_df$selected_day_prompts <- unlist(lapply(strsplit(as.character(day_df$selected_day_prompts), "_"), '[[', 1))

ggplot(day_df, aes(x=selected_day_prompts, y=Freq, fill=TYPE)) + geom_bar(stat = "identity") +
  labs(title="Random selection | Day n", x="\nQuestions", y="\nFrequency") +
  theme(axis.text.x=element_text(angle = 70, hjust = 1))





### Including set types
set_types = c("External", "Internal", "Reflective", "Reactive")

### Get the day-level question list first
external_questions = list.filter(question_list, type=="External")
internal_questions = list.filter(question_list, type=="Internal")
reflective_questions = list.filter(question_list, type=="Reflective")
reactive_questions = list.filter(question_list, type=="Reactive")

## No. of items External - 5(18%), Internal - 11(40%), Reflective - 6(21%), Reactive - 6(21%)
## Total questions in a day level pool - 10
get_questions_day = c(2, 4, 2, 2)

### Create an empty total prompt list
final_prompt_list = list()
selected_day_prompts = list()
question_df_with_day = data.frame(QUESTION = NA, DAY = NA)
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
    # day_level_list[j] <- questions_day[[rnd_index]]['id']
    
    rnd_completion = runif(1)
    if (rnd_completion <= completion_rate){
      # completion_counter = completion_counter + 1
      day_level_list[j] <- questions_day[[rnd_index]]['id']
      question_id <- questions_day[[rnd_index]]['id']$id
      day_into_study <- i
      question_df_with_day <- rbind(question_df_with_day, c(question_id, day_into_study))
    }
  }
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
  if (i == 1){
    selected_day_prompts = day_level_list 
  }
}

## Clear the NA row
question_df_with_day <- question_df_with_day[-1,]

question_df_with_day$QUESTION <- lapply(strsplit(as.character(question_df_with_day$QUESTION), "_"), '[[', 1)

## Convert to factors
question_df_with_day$QUESTION <- as.factor(unlist(question_df_with_day$QUESTION))
question_df_with_day$DAY <- as.factor(question_df_with_day$DAY)

## Create a contingency table with question and the day times
question_number_of_days <- aggregate(DAY ~ QUESTION, question_df_with_day, function(x) length(unique(x)))
question_number_of_days$DAY_PERCENT <- question_number_of_days$DAY/study_dur

question_number_of_days$TYPE <- lapply(question_number_of_days$QUESTION, get_type_for_df)
question_number_of_days$TYPE <- as.factor(unlist(question_number_of_days$TYPE))

# Write this to a file before plotting
write.csv(question_number_of_days, file = "D:/uEMA_Simulation_plots/Improved_plots/set_based_question_daywise_distribution.csv", sep = ",")

# Plot with number of days
ggplot(question_number_of_days, aes(x=QUESTION, y=DAY, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based selection | daywise distribution", x = "\nQuestions", y="\nNumber of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

ggplot(question_number_of_days, aes(x=QUESTION, y=DAY_PERCENT, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based selection | daywise distribution", x = "\nQuestions", y="\n% of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))
length(full_prompted_list)
random_df <- as.data.frame(prop.table(table(full_prompted_list)))

for (i in 1:nrow(random_df)){
  random_df$TYPE[i] <- get_type(as.character(random_df$full_prompted_list[i]), question_list)
}

random_df$full_prompted_list <- unlist(lapply(strsplit(as.character(random_df$full_prompted_list), "_"), '[[', 1))

## Plot distribution
ggplot(random_df, aes(x=full_prompted_list, y=Freq, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based selection", x = "\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

# Save the study duration prompt distribution file
write.csv(random_df, file="D:/uEMA_Simulation_plots/Improved_plots/set_based_prompting.csv", sep = ",")

## Plot for a given day's prompting
length(selected_day_prompts)
selected_day_prompts <- unlist(unlist(selected_day_prompts, recursive = FALSE))
length(unique(selected_day_prompts))
day_df <- as.data.frame(table(selected_day_prompts))
write(selected_day_prompts, file="D:/uEMA_Simulation_plots/Improved_plots/set_selection_list.txt", sep = ",")


for (i in 1:nrow(day_df)){
  day_df$TYPE[i] <- get_type(as.character(day_df$selected_day_prompts[i]), question_list)
}

day_df$selected_day_prompts <- unlist(lapply(strsplit(as.character(day_df$selected_day_prompts), "_"), '[[', 1))

ggplot(day_df, aes(x=selected_day_prompts, y=Freq, fill=TYPE)) + geom_bar(stat = "identity") +
  labs(title="Set-based selection | Day n", x="\nQuestions", y="\nFrequency") +
  theme(axis.text.x=element_text(angle = 70, hjust = 1))




## Use maxed out caps to prevent over prompting in a day
### Including set types
set_types = c("External", "Internal", "Reflective", "Reactive")

### Get the day-level question list first
external_questions = list.filter(question_list, type=="External")
internal_questions = list.filter(question_list, type=="Internal")
reflective_questions = list.filter(question_list, type=="Reflective")
reactive_questions = list.filter(question_list, type=="Reactive")

## No. of items External - 5(18%), Internal - 11(40%), Reflective - 6(21%), Reactive - 6(21%)
## Total questions in a day level pool - 10
get_questions_day = c(2, 4, 2, 2)

### Create an empty total prompt list
final_prompt_list = list()
selected_day_prompts = list()
question_df_with_day = data.frame(QUESTION = NA, DAY = NA)
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
    
    rnd_completion = runif(1)
    if (rnd_completion <= completion_rate){
      # completion_counter = completion_counter + 1
      day_level_list[j] <- questions_day[[rnd_index]]['id']
      
      question_id <- questions_day[[rnd_index]]['id']$id
      day_into_study <- i
      question_df_with_day <- rbind(question_df_with_day, c(question_id, day_into_study))
      
      quest_count = questions_day[[rnd_index]]['count']$count
      questions_day[[rnd_index]]['count']$count = quest_count + 1.0
    }
    
  }
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
  if (i == 1){
    selected_day_prompts = day_level_list 
  }
}

## Clear the NA row
question_df_with_day <- question_df_with_day[-1,]

question_df_with_day$QUESTION <- lapply(strsplit(as.character(question_df_with_day$QUESTION), "_"), '[[', 1)

## Convert to factors
question_df_with_day$QUESTION <- as.factor(unlist(question_df_with_day$QUESTION))
question_df_with_day$DAY <- as.factor(question_df_with_day$DAY)

## Create a contingency table with question and the day times
question_number_of_days <- aggregate(DAY ~ QUESTION, question_df_with_day, function(x) length(unique(x)))
question_number_of_days$DAY_PERCENT <- question_number_of_days$DAY/study_dur

question_number_of_days$TYPE <- lapply(question_number_of_days$QUESTION, get_type_for_df)
question_number_of_days$TYPE <- as.factor(unlist(question_number_of_days$TYPE))

# Write this to a file before plotting
write.csv(question_number_of_days, file = "D:/uEMA_Simulation_plots/Improved_plots/set_based_max_prompting_question_daywise_distribution.csv", sep = ",")

# Plot with number of days
ggplot(question_number_of_days, aes(x=QUESTION, y=DAY, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based + max filter | daywise distribution", x = "\nQuestions", y="\nNumber of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

ggplot(question_number_of_days, aes(x=QUESTION, y=DAY_PERCENT, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based + max filter | daywise distribution", x = "\nQuestions", y="\n% of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))
length(full_prompted_list)
random_df <- as.data.frame(prop.table(table(full_prompted_list)))

for (i in 1:nrow(random_df)){
  random_df$TYPE[i] <- get_type(as.character(random_df$full_prompted_list[i]), question_list)
}

random_df$full_prompted_list <- unlist(lapply(strsplit(as.character(random_df$full_prompted_list), "_"), '[[', 1))

## Plot distribution
ggplot(random_df, aes(x=full_prompted_list, y=Freq, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based + max filter selection", x = "\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

# Save the study duration prompt distribution file
write.csv(random_df, file="D:/uEMA_Simulation_plots/Improved_plots/set_based_mxed_out_prompting.csv", sep = ",")

## Plot for a given day's prompting
length(selected_day_prompts)
selected_day_prompts <- unlist(unlist(selected_day_prompts, recursive = FALSE))
length(unique(selected_day_prompts))
day_df <- as.data.frame(table(selected_day_prompts))
write(selected_day_prompts, file="D:/uEMA_Simulation_plots/Improved_plots/max_filter_list.txt", sep = ",")

for (i in 1:nrow(day_df)){
  day_df$TYPE[i] <- get_type(as.character(day_df$selected_day_prompts[i]), question_list)
}

day_df$selected_day_prompts <- unlist(lapply(strsplit(as.character(day_df$selected_day_prompts), "_"), '[[', 1))

ggplot(day_df, aes(x=selected_day_prompts, y=Freq, fill=TYPE)) + geom_bar(stat = "identity") +
  labs(title="Set-based + max filter selection | Day n", x="\nQuestions", y="\nFrequency") +
  theme(axis.text.x=element_text(angle = 70, hjust = 1))




## Also add minimum gap of 1 hour - i.e. 4 more prompts after each prompt
### Including set types
set_types = c("External", "Internal", "Reflective", "Reactive")

### Get the day-level question list first
external_questions = list.filter(question_list, type=="External")
internal_questions = list.filter(question_list, type=="Internal")
reflective_questions = list.filter(question_list, type=="Reflective")
reactive_questions = list.filter(question_list, type=="Reactive")

## No. of items External - 5(18%), Internal - 11(40%), Reflective - 6(21%), Reactive - 6(21%)
## Total questions in a day level pool - 10
get_questions_day = c(2, 4, 2, 2)

### Create an empty total prompt list
final_prompt_list = list()
selected_day_prompts = list()
question_df_with_day = data.frame(QUESTION = NA, DAY = NA)
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
    questions_day[[ind]]["recent_index"] <- 0
  }
  
  day_level_list = list()
  
  for (j in 1:total_prompts_day){
    ## Filter out already maxed out prompts
    questions_day <- list.filter(questions_day, count < max_prompts_per_day)
    
    ## Keep a copy of the eliminated list
    filtered_by_gap_questions <- list()
    if (j <= 4){
      ### Remove any question with the non-zero recent_index
      filtered_by_gap_questions <- list.filter(questions_day, recent_index !=0)
      questions_day <- list.filter(questions_day, recent_index == 0)
    } else if (j > 4 && j <= total_prompts_day - 4) {
      filtered_by_gap_questions <- list.filter(questions_day, recent_index > j - 4)
      questions_day <- list.filter(questions_day, recent_index <= j - 4)
    }
    
    # print(paste0("In filtered list: ", length(questions_day)))
    
    total_current_questons = length(questions_day)
    
    ## Generate a random number between 1 - total questions
    rnd_index = sample(1:total_current_questons, 1)
    rnd_completion = runif(1)
    if (rnd_completion <= completion_rate){
      # completion_counter = completion_counter + 1
      day_level_list[j] <- questions_day[[rnd_index]]['id']
      quest_count = questions_day[[rnd_index]]['count']$count
      
      question_id <- questions_day[[rnd_index]]['id']$id
      day_into_study <- i
      question_df_with_day <- rbind(question_df_with_day, c(question_id, day_into_study))
      
      questions_day[[rnd_index]]['count']$count = quest_count + 1.0
      questions_day[[rnd_index]]['recent_index']$recent_index = j
    }
    
    ## Add the filtered by gap questions back to the list
    questions_day <- c(questions_day, filtered_by_gap_questions)
    
    
  }
  
  final_prompt_list[[length(final_prompt_list) + 1]] <- day_level_list
  if (i == 1){
    selected_day_prompts = day_level_list 
  }
}

## Clear the NA row
question_df_with_day <- question_df_with_day[-1,]

question_df_with_day$QUESTION <- lapply(strsplit(as.character(question_df_with_day$QUESTION), "_"), '[[', 1)

## Convert to factors
question_df_with_day$QUESTION <- as.factor(unlist(question_df_with_day$QUESTION))
question_df_with_day$DAY <- as.factor(question_df_with_day$DAY)

## Create a contingency table with question and the day times
question_number_of_days <- aggregate(DAY ~ QUESTION, question_df_with_day, function(x) length(unique(x)))
question_number_of_days$DAY_PERCENT <- question_number_of_days$DAY/study_dur

question_number_of_days$TYPE <- lapply(question_number_of_days$QUESTION, get_type_for_df)
question_number_of_days$TYPE <- as.factor(unlist(question_number_of_days$TYPE))

# Write this to a file before plotting
write.csv(question_number_of_days, file = "D:/uEMA_Simulation_plots/Improved_plots/set_based_max_mingap_question_daywise_distribution.csv", sep = ",")

# Plot with number of days
ggplot(question_number_of_days, aes(x=QUESTION, y=DAY, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based + Max filter + Min gap | daywise distribution", x = "\nQuestions", y="\nNumber of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

ggplot(question_number_of_days, aes(x=QUESTION, y=DAY_PERCENT, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based + Max filter + Min gap | daywise distribution", x = "\nQuestions", y="\n% of days") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

length(final_prompt_list)
full_prompted_list <- unlist(final_prompt_list, recursive = FALSE)
full_prompted_list <- unlist(full_prompted_list)
length(unique(full_prompted_list))
length(full_prompted_list)
random_df <- as.data.frame(prop.table(table(full_prompted_list)))

for (i in 1:nrow(random_df)){
  random_df$TYPE[i] <- get_type(as.character(random_df$full_prompted_list[i]), question_list)
}

random_df$full_prompted_list <- unlist(lapply(strsplit(as.character(random_df$full_prompted_list), "_"), '[[', 1))

## Plot distribution
ggplot(random_df, aes(x=full_prompted_list, y=Freq, fill=TYPE)) + geom_bar(stat="identity") + 
  labs(title = "Set-based + max filter + min gap", x = "\nQuestions", y="\nFrequency(%)") +
  theme(axis.text.x = element_text(angle=70, hjust=1))

# Save the study duration prompt distribution file
write.csv(random_df, file="D:/uEMA_Simulation_plots/Improved_plots/set_based_max_out_min_gap_prompting.csv", sep = ",")

## Plot for a given day's prompting
length(selected_day_prompts)
selected_day_prompts <- unlist(unlist(selected_day_prompts, recursive = FALSE))
length(unique(selected_day_prompts))
day_df <- as.data.frame(table(selected_day_prompts))

### Write the list to a file
write(selected_day_prompts, file="D:/uEMA_Simulation_plots/Improved_plots/min_gap_day_list.txt", sep = ",")

for (i in 1:nrow(day_df)){
  day_df$TYPE[i] <- get_type(as.character(day_df$selected_day_prompts[i]), question_list)
}

day_df$selected_day_prompts <- unlist(lapply(strsplit(as.character(day_df$selected_day_prompts), "_"), '[[', 1))

ggplot(day_df, aes(x=selected_day_prompts, y=Freq, fill=TYPE)) + geom_bar(stat = "identity") +
  labs(title="Set-based + max filter + min gap | Day n", x="\nQuestions", y="\nFrequency") +
  theme(axis.text.x=element_text(angle = 70, hjust = 1))

