############# return a new slide function script #############

# This is where all the magic happens

library(tibble)
library(dplyr) 
library(catR)
library(here)

items = read.csv(here("data", "item_difficulty.csv")) %>% 
  dplyr::select(target, itemDifficulty = Item.Difficulty, discrimination = Discrimination, slide_num) %>%
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

item_key = read.csv(here("data", "item_difficulty.csv")) %>% 
  dplyr::select(target, slide_num, itemDifficulty = Item.Difficulty)

starting_items <- c(130, 25, 39, 154) # are 0.02 or -0.02. closest to 0.

# the magic!

irt_function <- function(all_items, IRT = T, previous = NULL){

      # this is for the out argument. 
      # creates a vector of the items that have already been completed
      # to be fed to IRT so they don't get chosen again
      completed = all_items %>% 
        drop_na(response) %>%
        pull(item_number)
      
      # don't re-use previous items
      if(!is.null(previous)){
        previously_completed = previous %>%
          drop_na(response) %>%
          pull(item_number)
          
        completed = c(completed, previously_completed)
      }
      
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
       # standard error of the mean
       sem = semTheta(ability, bank, x)
       
      #print(fullDist(th = ability, it = bank, method = "EAP", range = c(-5, 5)))
    
       if(IRT){
         
         next_item = if(length(completed)<175){
           nextItem(itemBank = bank, theta = ability, out = completed)
         } else {
           NA
         }
         
       # save to a list to return to the app
       #   tmp_list = list()
       # tmp_list[[1]] = ability
       # tmp_list[[2]] = next_item
       # tmp_list[[3]] = sem
       
         tmp_list = list(
         ability,
         next_item,
         sem
         )
        
       return(tmp_list)
       
    } else {
      # randomize? if true, then use random order column

      next_slide_num <- all_items %>%
        mutate(next_item = ifelse(!is.na(response), pnt_order+1, NA)) %>%
        filter(pnt_order == max(next_item, na.rm = T)) 
      
      tmp_list = list(
        ability,
        list(
          NA,
          slide_num_out = ifelse(nrow(next_slide_num) < 1, 190, next_slide_num$slide_num)
          ),
        sem
      )
      
      return(tmp_list)
      
    }
}

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






