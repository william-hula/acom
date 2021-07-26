############# return a new slide function script #############

# This is where all the magic happens


# the magic!

irt_function <- function(all_items, IRT = T, exclude_previous = F, previous, test = NA){

      # this is for the out argument. 
      # creates a vector of the items that have already been completed
      # to be fed to IRT so they don't get chosen again
      completed = all_items %>% 
        drop_na(response) %>%
        pull(item_number)
      
      # don't re-use previous items
      if(exclude_previous){
        previously_completed = previous %>%
          # this section limits ignoring to only the previous test
                                group_by(date) %>%
                                mutate(num = cur_group_id()) %>%
                                filter(num == max(num)) %>%
        # selects only done items and grabs them.
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
       
       if(IRT){
         
         next_item = if(length(completed)<175){
           nextItem(itemBank = bank, theta = ability, out = completed)
         } else {
           NA
         }
       
         print(next_item)
         
         tmp_list = list(
         ability,
         next_item,
         sem
         )
        
       return(tmp_list)
       
    } else if(test == "walker") {
      # randomize? if true, then use random order column
      next_slide_num <- all_items %>%
        mutate(next_item = ifelse(!is.na(response), walker_order+1, NA)) %>%
        filter(walker_order == max(next_item, na.rm = T)) 
      
      tmp_list = list(
        ability,
        list(
          NA,
          slide_num_out = ifelse(nrow(next_slide_num) < 1, 190, next_slide_num$slide_num)
        ),
        sem
      )
      
      return(tmp_list)
      
    } else {
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







