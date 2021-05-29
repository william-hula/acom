###########

# This script runs each response from the observed 24 participants through the app
# right now its not using key presses, but a radio button and action button
# because I can't figure out how to input the key presses in the unit test. 
# Basically, there are hidden inputs (will never be shown) that take the same
# input "1" and "2"
# the inputs are the same otherwise though. 
# it returns a .csv file for each participant and saves it in the tests tests_output folder
# there is another script called "process_test_data.R"
# that processes the output of this unit test. 

## !!!! Call this script using shinytest.R 

# things to change to enable test to run:
# app title in text.R
# reset("keys)


library(here)
library(tidyverse)
#library(shinytest)

# observed data
observed <- read_csv(here("validation", "validation.csv")) %>%
  # only need rows with testing
  filter(modelType == "3pl",
         exam == "PNT-CAT30") %>%
  mutate(response = ifelse(response == "correct", "2", "1")) %>%
  select(examinee, response)

examinees <- unique(observed$examinee)

for(i in examinees){
# start the test
app <- ShinyDriver$new(here())
# give it a name
app$snapshotInit("mytest")
# name is the examinee name
app$setInputs(name = i)
# ntoes is just notes. 
app$setInputs(notes = "notes")
# 30 items
app$setInputs(numitems = "30")
# the date on teh unit test is a little funky. I've just left it
# it doesn't matter. its not tracked
#app$setInputs(date = 18769)

# click to get started
app$setInputs(start_practice = "click")

pr_resp = rep(c("1", "2"), 6)
for(n in pr_resp){
  app$setInputs(keys = n)
  app$setInputs(enter_key = "click")
}

app$setInputs(start = "click")

# what responses do you want
# subsets the observed responses in order

responses <- observed$response[observed$examinee==i]
print(length(responses))
  # key presses (fed to a radio button and action button for now)
  for(p in 1:length(responses)){
    app$setInputs(keys = responses[p], wait_=FALSE, values_=FALSE)
    app$setInputs(enter_key = "click")
  }

app$snapshot()
# grabs the values at the end of the test. 
# this refers to a new chunk in the server side "exportTestValues" currently
# around line 340
exports <- app$getAllValues()[[3]]
df <- tibble(
  discrimination = strsplit(exports$discrimination, "_", 1)[[1]],
  target = strsplit(exports$words, "_")[[1]],
  item_number = strsplit(exports$item_number, "_")[[1]],
  order = strsplit(exports$order, "_")[[1]],
  itemDifficulty = strsplit(exports$itemDifficulty, "_")[[1]],
  key_press = strsplit(exports$key_press, "_")[[1]],
  response = strsplit(exports$responses, "_")[[1]],
  ability = strsplit(exports$abil, "_", 1)[[1]],
  sem = strsplit(exports$sem, "_")[[1]],
  name = i
)
# save the .csv file

write.csv(df, here("tests", "test_output", Sys.Date(), paste0(i, "_test_dat.csv")))
}




