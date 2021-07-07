# gets first item. 

get_first_item <- function(previous_dat = NULL){
  
  choices = c(130, 25, 39, 154)
  
  if(is.null(previous_dat)){
    
    # if we want to sample the four anyway
    # first_item = sample(choices, 1)
    # return(first_item)
    return(130)
    
  } else {
    
    first_item <- 
      tibble(choices = choices) %>%
      filter(choices %!in% previous_dat$slide_num) %>%
      slice_sample(n = 1) %>%
      pull(choices)
    
    print(first_item)
    return(first_item)
  }
  
  
}
