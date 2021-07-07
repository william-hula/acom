
#### This script processes the output of the testing 
# cleans up the data and combines it with the observed data
# makes some comparisons to the observed data. 

# right now there are two differences between this shiny app and the observed data

# one, there are ties in the item difficulty bank
# and it looks like our app is randomly choosing the ties
# not sure what catpuccino was doing, but that's causing some discrepancies
# in the items that are presented, but not the final numbers

# second, there is a tiny bit of difference in ability estimates and sem estimates
# honestly, my guess is just rounding error somewhere. 
# the correlations between the estimates are in the .999 range
# the agreement is essentially ~= 1 as well. 
# agreement on the error is also very high but has an oddly lower bound CI
# need to track down why that might be. I suspect it has to do with a limited range of values anyway

# read in test data
library(fs)
library(vroom)
library(tidyr)
library(stringr)
files <- fs::dir_ls(here("tests", "test_output", Sys.Date()))
files <- fs::dir_ls(here("tests", "test_output", "2021-06-01"))

df = vroom::vroom(files)
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

mean(together$target_test)
# [1] 0.7611111

# when targets don't match, whats the maximum difference between difficulties?
# oh, its zero, they are the same difficulty
max(together$difficulty_test[together$target_test==0])
# [1] 0


# we can look at some histograms of the differences for ability and error:
hist(together$ability_test, breaks = 20)
hist(together$error_test, breaks = 20)  

# lets just look at the final estimate too:
final <- together %>%
  filter(order == 30) %>%
  mutate(examinee_deid = paste0("participant_", row_number())) %>%
  select(examinee_deid, ability, error, obs_ability, obs_error)

#write.csv(final, here("tests", "2021-05-22_shiny_vs_observed.csv"))

# these suggest no problems as well
cor(final$ability, final$obs_ability)
plot(final$ability, final$obs_ability)
plot(final$error, final$obs_error)
cor(final$error, final$obs_error)

# and we can calculate some quick agreement scores for final ability and error:
# to make sure there's no bias

irr::icc(tibble(a = final$ability, b = final$obs_ability),
         model = "twoway",
         type = "agreement")

irr::icc(tibble(a = final$error, b = final$obs_error),
         model = "twoway",
         type = "agreement")

