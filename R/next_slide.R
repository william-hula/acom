############# return a new slide function script #############

# This is where all the magic happens

library(tibble)
library(dplyr) 
library(catR)
library(here)

# we don't need these right?
# library(catIrt)
# library(irtoys) 
# library(ltm)

items = read.csv(here("www", "item_difficulty.csv")) %>% 
  dplyr::select(target, itemDifficulty = Item.Difficulty, discrimination = Discrimination, slide_num) %>%
  mutate(response = NA,
         item_number = row_number())

item_key = read.csv(here("www", "item_difficulty.csv")) %>% 
  dplyr::select(target, slide_num, itemDifficulty = Item.Difficulty)

# the magic!

irt_function <- function(all_items){
    tmp_list = list()
    
    # this is for the out argument. 
    # creates a vector of the items that have already been completed
    # to be fed to IRT so they don't get chosen again
    completed = all_items %>% 
      drop_na(response) %>%
      pull(item_number)
    
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
     ability = thetaEst(bank, x, method = "BM")
     # generates the next item
     
      next_item = if(length(completed)<175){
        nextItem(itemBank = bank, theta = ability, out = completed)
     } else {
        NA
     }
     # standard error of the mean
     sem = semTheta(ability, bank, x)
     
     # save to a list to return to the app
     tmp_list[[1]] = ability
     tmp_list[[2]] = next_item
     tmp_list[[3]] = sem
    
    return(tmp_list)
}


###### Want to test this function? 

#1. Read in items above. 

#2. Create some random responses:

# e.g.:
# items[3,5] = 0
# items[4,5] = 0
# items[5,5] = 0
# items[50,5] = 1
# items[60,5] = 0

#3. fun the function. it just takes in this data frame with some responses.

#irt_function(items)
