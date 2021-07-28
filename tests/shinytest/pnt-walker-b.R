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
shhh <- suppressPackageStartupMessages # It's a library, so shhh!
suppressWarnings(
  shhh({
    library(shinytest)
    library(here)
    library(tidyverse)
    library(progress)
  })
)

suppressWarnings(
walker <- read_csv(col_types = cols(), here("data", "item_difficulty.csv")) %>%
  select(item = target, walker, walker_order)
)

# who has a full test
full_test_id = read_csv(col_types = cols(), here("validation", "validation.csv")) %>%
  filter(modelType == "3pl") %>%
  count(examinee) %>%
  filter(n == 175) %>%
  pull(examinee)

# observed data
observed <- read_csv(col_types = cols(), here("validation", "validation.csv")) %>%
  # only need rows with testing
  filter(modelType == "3pl",
         examinee %in% full_test_id) %>%
  mutate(response = ifelse(response == "correct", "2", "1")) %>%
  left_join(walker, by = "item") %>%
  filter(walker == "B") %>%
  arrange(examinee, walker_order) %>%
  select(examinee, item, walker, walker_order, response)
  
examinees <- unique(observed$examinee)
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
  app$snapshotInit("pnt-walker-b")
  # name is the examinee name
  app$setInputs(name = i)
  # ntoes is just notes. 
  app$setInputs(notes = "notes")
  # next intro slide
  app$setInputs(glide_next1 = "click")
  # 30 items
  app$setInputs(numitems = "walker")
  # form A
  app$setInputs(walker = "B")
  # next
  app$setInputs(glide_next2 = "click")
  
  # click to get started
  app$setInputs(start_practice = "click")
  
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
  
  write.csv(df, here("tests", "test_output", "walker_b", Sys.Date(), paste0(i, "_test_dat.csv")))
  pb$tick()
}

# pause for saving. 
Sys.sleep(1)

# read in test data
suppressWarnings(
  shhh({
    library(fs)
    library(vroom)
    library(tidyr)
    library(stringr)
    library(testthat)
    
  })
)
files <- fs::dir_ls(here("tests", "test_output", "walker_b", Sys.Date()))

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
         examinee %in% full_test_id) %>%
  mutate(response = ifelse(response == "correct", 0, 1)) %>%
  left_join(walker, by = "item") %>%
  filter(walker == "B") %>%
  hablar::retype() %>%
  group_by(examinee) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  select(examinee, order, target = item, discrimination, itemDifficulty = difficulty, response, ability, error) %>%
  # identify the order to select last item later on. 
  rename_with(~str_c("obs_", .), 4:8)

# join them together. look at differences between numeric values
together <- observed %>%
  full_join(shiny, by = c("examinee", "target")) %>%
  select(examinee, target, response, obs_response)

write.csv(together, here("tests", "agreement_data", paste0(Sys.Date(), "_walker-b_shiny_vs_observed.csv")))

test_that("Walker B all responses", {
  print("Walker B; test of equivalent responses")
  expect_true(
    sum(together$obs_response == together$response) == nrow(together)
  )
})
