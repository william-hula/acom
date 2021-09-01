# validation

# Script evaluates final estimates of the CATR IRT algorithm and Fergadiotis, 2015

library(here)
library(tidyverse)
source(here('R', 'next_slide.R'))
library(catR)

# observed data
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

# IRT function from the app
# The next_slide bit is not done
irt_function <- function(all_items){
  tmp_list = list()
  
  # this is for the out argument. 
  # creates a vector of the items that have already been completed
  # to be fed to IRT so they don't get chosen again
  # completed = all_items %>% 
  #   drop_na(response) %>%
  #   pull(item_number)
  
  
  # dataframe of inputs
  pars = data.frame(a = all_items$discrimination,
                    b = all_items$itemDifficulty,
                    c = rep(1), #1PL has no guessing parameter ,
                    d = rep(0), #1PL has no innatention parameter,
                    cbGroup = rep(1))
  
  # breaks it down into what gets fed into the 1PL IRT
  prov = breakBank(pars)
  bank = prov$itemPar
  rownames(bank) <- all_items$target
  x = all_items$response
  
  # ability estimate using bayes modal:
  ability = thetaEst(bank, x, method = "EAP", range = c(-5, 5))
  # generates the next item
  # 
  #next_item = if(length(completed)<175){
  # next_item = nextItem(itemBank = bank, theta = ability, out = completed)
  #} else {
  #  NA
  #}
  # standard error of the mean
  sem = semTheta(ability, bank, x)
  
  # save to a list to return to the app
  tmp_list[[1]] = ability
  #tmp_list[[2]] = next_item
  tmp_list[[2]] = sem
  
  return(tmp_list)
}  


observed_nest <- observed %>%
  nest_by(examinee)

observed_nested <- observed_nest %>%
  mutate(ability_new = irt_function(data)[1],
         error_new = irt_function(data)[2]) %>%
  unnest(cols = c(data, ability_new, error_new)) %>%
  filter(n == 30) %>%
  mutate(across(contains("ability"), as.numeric),
         across(contains("error"), as.numeric))

cor(observed_nested$ability, observed_nested$ability_new)
cor(observed_nested$error, observed_nested$error_new)

plot(observed_nested$ability, observed_nested$ability_new)
plot(observed_nested$error, observed_nested$error_new)



         