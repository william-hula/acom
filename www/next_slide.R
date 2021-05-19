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
items = read.csv("www/item_difficulty.csv") %>% arrange(Item.Difficulty) %>%
  select(target, diff = Item.Difficulty, slide_num)

next_slide <- function (resp, df) {
  
  # resp will be a "1" or "2" based on the key input. 
  # change to 0 or 1
  tmp = response = ifelse(resp == "1", "0",
                          ifelse( resp == "2", "1", "NR")
  )
  
  
  # This is the area where you calculate a new ability estimate!!!!!!!!!!1
  # -----------------------------------------------------------------------------
  # if 0, then....else if 1 then...
  # third condition shouldn't be able to happen, but just in case...
  if (tmp == 0){
    est = runif(1, min = -1.8, max = 0)
  } else if (tmp == 1) {
    est = runif(1, 0, 1.8)
  } else {
    est = NA
  }
  # -----------------------------------------------------------------------------
  
  # find the item closest to a random estimate
  item_difficulty_2 <- df %>%
    mutate(est = est,
           difference = abs(diff - est)) %>%
    # top row has lowest absolute difference. 
    arrange(difference) %>%
    # grabs top row
    slice(1)
    
  # return a 1 row dataframe that can be accessed. 
  return(item_difficulty_2)
  
}
