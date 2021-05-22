

# function which takes the name of a participant and their responses. 
# assumes that the chosen items will be the same as what has been previously done
# not sure how to get around that yet
library(tidyverse)
library(here)
library(shinytest)

### data
observed <- read_csv(here("validation", "validation.csv")) %>%
  # only need rows with testing
  filter(modelType == "3pl",
         exam == "PNT-CAT30") %>%
  # correct is 0, incorrect is 1. don't ask me why
  mutate(response = ifelse(response == "correct", 0, 1)
  ) %>%
  select(examinee, target = item, discrimination, itemDifficulty = difficulty, response, ability, error) %>%
  # identify the order to select last item later on. 
  group_by(examinee) %>%
  mutate(n = row_number()) %>%
  ungroup() 

examinees <- c("1504_5_15", "1504_5_19", "1504_5_25")

automated_run <- function(examinee){
app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$setInputs(name = "name")
app$setInputs(notes = "notes")
app$setInputs(numitems = "30")
app$setInputs(date = 18769)
app$setInputs(start = "click")

# what responses do you want
responses <- observed$response[observed$examinee==examinee]

# key presses
for(i in 1:length(responses)){
  app$setInputs(keys = responses[i])
  app$setInputs(enter_key = "click")
}
#app$snapshot()
vals <- app$getAllValues()

#exports <- vals[[3]]

# df <- tibble(
#   discrimination = strsplit(exports$discrimination, "_", 1)[[1]],
#   target = strsplit(exports$words, "_")[[1]],
#   item_number = strsplit(exports$item_number, "_")[[1]],
#   order = strsplit(exports$order, "_")[[1]],
#   itemDifficulty = strsplit(exports$itemDifficulty, "_")[[1]],
#   key_press = strsplit(exports$key_press, "_")[[1]],
#   response = strsplit(exports$responses, "_")[[1]],
#   ability = strsplit(exports$abil, "_", 1)[[1]],
#   sem = strsplit(exports$sem, "_")[[1]]
# )
write_rds(vals, here("tests", paste0(examinee, "_test_dat.RDS")))
}

for(i in seq_along(examinees)){
  automated_run(i)
}


  



