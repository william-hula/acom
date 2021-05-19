############# return a new slide function script #############

# this is a function to simulate an IRT algorithm
# it takes the input of the previous item
# if the item is incorrect, samples a uniform distribution that is between -1.8 and 0
# if correct, samples a uniform distribution between 0 and 1.8
# then pulls the item from the item bank that is closest in difficulty to the sampled number
# the r script in the app then deletes that item from being pulled again. (not in this function)
# it returns a row of a data frame with the chosen target name, difficulty, slide number, est and difference between the 
# randomly drawn value and the difficulty. 


next_slide <- function (resp) {
  
  
  tmp = response = ifelse(resp == "1", "0",
                          ifelse( resp == "2", "1", "NR")
  )
  
  if (tmp == 0){
    est = runif(1, min = -1.8, max = 0)
  } else if (tmp == 1) {
    est = runif(1, 0, 1.8)
  } else {
    est = NA
  }
  
  item_difficulty_2 <- item_difficulty %>%
    mutate(est = est,
           difference = abs(diff - est)) %>%
    arrange(difference) %>%
    slice(1)
    
  return(item_difficulty_2)
  
}
