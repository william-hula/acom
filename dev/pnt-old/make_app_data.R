
################## PACKAGES ###################

#!!!!!!!!!!!! Make sure each of these is listed in the description file!!!
library(here)
library(tidyverse)
######## DEFINE GLOBAL VARIABLES ########

# These indicate errors (1) and correct responses (2)
incorrect_key_response = "1"
correct_key_response = "2"

response_keys <- c(
  incorrect_key_response, correct_key_response
)

# The next button
enter <- c("enter", "space")
end_test_key = "esc"


items = read.csv(here("dev", "pnt-old", "data", "item_difficulty.csv")) %>% 
  dplyr::select(target, itemDifficulty = Item.Difficulty, discrimination = Discrimination, slide_num, walker, walker_order) %>%
  arrange(slide_num)%>%
  mutate(response = NA,
         item_number = row_number(),
         order = NA,
         key = NA,
         resp = NA,
         ability = NA,
         sem = NA,
         pnt_order = row_number()
  )

item_key = read.csv(here("dev", "pnt-old", "data", "item_difficulty.csv")) %>% 
  dplyr::select(target, slide_num, itemDifficulty = Item.Difficulty, walker, walker_order)

starting_items <- c(130, 25, 39, 154) # are 0.02 or -0.02. closest to 0.


### UPDATE TO T SCALE. importing new file and replacing old item difficulty and discrimination columns with new estimates
# updated 10/6/21

tscale <- read.csv(here("dev","pnt-old", "data", "pnt_Tscaled_item_parameters_2021_10_04.csv"))
colnames(tscale) <- c("ignore", "id", "target", "discrimination", "itemDifficulty")
tscale<-tscale[,3:5]

items <- items %>% 
  dplyr::select(-itemDifficulty, -discrimination) %>%
  left_join(tscale, by = "target") %>%
  select(target, itemDifficulty, discrimination, slide_num:pnt_order)

item_key <- items %>%
  dplyr::select(target, slide_num, itemDifficulty, walker, walker_order) %>%
  arrange(target)

### thetas for generating the results histogram:

thetas <- read_csv(here("dev","pnt-old", "data", "thetas_Tscaled_MAPPDn296_R03n39_2021_10_04.csv"))$mean

# Saves the data as an internal data file. 
# uncomment to update app data. 

download_df = items[c("item_number", "target", "key")]
download_df$test = "offline"


jsCode <- 'shinyjs.gettime = function(params) {
     var time = Date();
     Shiny.onInputChange("jstime", time);
   }' 

# usethis::use_data(internal = T, overwrite = T,
#                   item_key,
#                   items,
#                   correct_key_response,
#                   incorrect_key_response,
#                   end_test_key,
#                   enter,
#                   response_keys,
#                   starting_items,
#                   thetas,
#                   download_df,
#                   jsCode
#                   )

