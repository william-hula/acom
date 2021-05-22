
library(here)
library(tidyverse)

# observed data
observed <- read_csv(here("validation", "validation.csv")) %>%
  # only need rows with testing
  filter(modelType == "3pl",
         exam == "PNT-CAT30") %>%
  mutate(response = ifelse(response == "correct", "2", "1")) %>%
  select(examinee, response)

examinees <- unique(observed$examinee)

for(i in examinees){
app <- ShinyDriver$new(here())
app$snapshotInit("mytest")

app$setInputs(name = i)
app$setInputs(notes = "notes")
app$setInputs(numitems = "30")
#app$setInputs(date = 18769)
app$setInputs(start = "click")

# what responses do you want
responses <- observed$response[observed$examinee==i]

  # key presses
  for(p in 1:length(responses)){
    app$setInputs(keys = responses[p])
    app$setInputs(enter_key = "click")
  }

#app$snapshot()
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
write.csv(df, here("tests", "test_output", paste0(i, "_test_dat.csv")))
}




