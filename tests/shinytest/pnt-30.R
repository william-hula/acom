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

library(shinytest)
library(here)
library(tidyverse)
library(progress)

# observed data
observed <- read_csv(here("validation", "validation.csv")) %>%
  # only need rows with testing
  filter(modelType == "3pl",
         exam == "PNT-CAT30") %>%
  mutate(response = ifelse(response == "correct", "2", "1")) %>%
  select(examinee, response)

examinees <- unique(observed$examinee)[1]
print(paste0("The number of participants to test is ", length(examinees)))
pb <- progress_bar$new(
  format = "  testing in progress [:bar] :current/:total in :elapsed",
  total = length(examinees), clear = FALSE, width= 60, force = T)
pb$tick(0)

i = examinees
for(i in examinees){
# start the test
app <- ShinyDriver$new(here())
# give it a name
app$snapshotInit("pnt-30")
# name is the examinee name
app$setInputs(name = i)
# ntoes is just notes. 
app$setInputs(notes = "notes")
# next intro slide
app$setInputs(glide_next1 = "click")
# 30 items
app$setInputs(numitems = "30")
# next
app$setInputs(glide_next2 = "click")

# click to get started
app$setInputs(start_practice = "click")
app$snapshot()

pr_resp = rep(c("1", "2"), 6)
for(n in pr_resp){
  app$setInputs(keys = n, wait_=FALSE, values_=FALSE)
  app$setInputs(enter_key = "click")
}

app$setInputs(start = "click")

# what responses do you want
# subsets the observed responses in order

responses <- observed$response[observed$examinee==i]

  # key presses (fed to a radio button and action button for now)
  for(p in 1:length(responses)){
    app$setInputs(keys = responses[p])
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
pb$tick()
}

# pause for saving. 
Sys.sleep(1)

# read in test data
library(fs)
library(vroom)
library(tidyr)
library(stringr)
files <- fs::dir_ls(here("tests", "test_output", Sys.Date()))

df = suppressMessages(
  vroom::vroom(files)
)
rm(files)

# process it
shiny <- df %>%
  select(examinee=name, order, target, discrimination, itemDifficulty, response, ability, error = sem)

# read in bserved data from R03 and process it
observed <- read.csv(here("validation", "validation.csv")) %>%
  # only need rows with testing
  filter(modelType == "3pl",
         exam == "PNT-CAT30") %>%
  mutate(response = ifelse(response == "correct", 0, 1)) %>%
  hablar::retype() %>%
  group_by(examinee) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  select(examinee, order, target = item, discrimination, itemDifficulty = difficulty, response, ability, error) %>%
  # identify the order to select last item later on. 
  rename_with(~str_c("obs_", .), 3:8)

# join them together. look at differences between numeric values
together <- observed %>%
  full_join(shiny, by = c("examinee", "order")) %>%
  mutate(target_test = ifelse(obs_target==target, 1, 0),
         ability_test = obs_ability - ability,
         error_test = obs_error - error,
         difficulty_test = obs_itemDifficulty - itemDifficulty)

# note that the target has not that great percent agreement:
# however, for targets taht are not the same, the item difficulties 
# are always equal, suggesting that the difference is how the algorithms are dealing
# with ties. 

# lets just look at the final estimate too:
final <- together %>%
  filter(order == 30) %>%
  mutate(examinee_deid = paste0("participant_", row_number())) %>%
  select(examinee_deid, ability, error, obs_ability, obs_error)

write.csv(final, here("tests", "agreement_data", "2021-05-22_shiny_vs_observed.csv"))

ab = irr::icc(tibble(a = final$ability, b = final$obs_ability),
              model = "twoway",
              type = "agreement")

sem = irr::icc(tibble(a = final$error, b = final$obs_error),
               model = "twoway",
               type = "agreement")

print(ab)

print(sem)

library(testthat)
test_that("agreement is excellent", {
           expect_gt(ab$value, 0.99)
           expect_gt(sem$value, 0.99)
})


