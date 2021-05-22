# processing ability out
library(fs)
library(vroom)
files <- fs::dir_ls(here("tests", "test_output"))
df = vroom::vroom(files)
rm(files)

shiny <- df %>%
  select(examinee=name, order, target, discrimination, itemDifficulty, response, ability, error = sem)
# observed data
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

together <- observed %>%
  full_join(shiny, by = c("examinee", "order")) %>%
  mutate(target_test = ifelse(obs_target==target, 1, 0),
         ability_test = obs_ability - ability,
         error_test = obs_error - error,
         difficulty_test = obs_itemDifficulty - itemDifficulty)


hist(together$ability_test, breaks = 20)
hist(together$error_test, breaks = 20)  

final <- together %>%
  filter(order == 30)

cor(final$ability, final$obs_ability)
plot(final$ability, final$obs_ability)

