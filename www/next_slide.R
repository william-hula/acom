############# return a new slide function script #############

# this is a function to simulate an IRT algorithm
# it takes the input of the previous item
# if the item is incorrect, samples a uniform distribution that is between -1.8 and 0
# if correct, samples a uniform distribution between 0 and 1.8
# then pulls the item from the item bank that is closest in difficulty to the sampled number
# the r script in the app then deletes that item from being pulled again. (not in this function)
# it returns a row of a data frame with the chosen target name, difficulty, slide number, est and difference between the 
# randomly drawn value and the difficulty. 
library(tibble)
library(dplyr)
library(catIrt)
library(irtoys)
library(ltm)
library(catR)

items = read.csv("www/item_difficulty.csv") %>% 
  dplyr::select(target, itemDifficulty = Item.Difficulty, discrimination = Discrimination, slide_num) %>%
  mutate(response = NA)

item_key = read.csv("www/item_difficulty.csv") %>% 
  dplyr::select(target, slide_num, itemDifficulty = Item.Difficulty)


irt_function <- function(all_items){
    tmp_list = list()
    pars = data.frame(a = all_items$discrimination,
                      b = all_items$itemDifficulty,
                      c = rep(1), #1PL has no guessing parameter ,
                      d = rep(0), #1PL has no innatention parameter,
                      cbGroup = rep(1))
    
    prov = breakBank(pars)
    bank = prov$itemPar
    rownames(bank) <- all_items$target
    x = all_items$response
     ability = thetaEst(bank, x, method = "BM")
     next_item = nextItem(itemBank = bank, theta = ability)
     tmp_list[[1]] = ability
     tmp_list[[2]] = next_item
    
    return(tmp_list)
  }
