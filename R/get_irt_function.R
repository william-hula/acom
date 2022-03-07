
#' The IRT magic function
#'
#' This function is the core of the computer adaptive functionality. It takes a data
#' frame of all the current items, and whether or not the current test is adaptive (IRT)
#' whether or not the user has elected to exclude previous items, what those items were,
#' whether or not the user elected to exclude eskimo, and whether the test is a 30-item static short 
#' form. 
#' 
#' First, it creates a vector of all of the completed items in the current test
#' Then adds any items from a previous test if there was one. 
#' Then uses the current test responses to generate an ability estimate, sem, and the next item
#' How these are generated depends on the current test (adaptive or not, 175 or not, walker or not)
#'
#' @param all_items a dataframe of all items in the current test with responses appended
#' @param IRT whether or not the test is adaptive
#' @param exclude_previous excluding items used in a prior administration
#' @param previous what those previous items were
#' @param exclude_eskimo eclude the eskimo item for cultural sensitivity
#' @param walker is it a walker short form test
#' @return A list with ability first then the next item then the sem
#' @export
irt_function <- function(all_items, IRT = T, exclude_previous = F, previous, exclude_eskimo = T, walker = F){

      ##############################################################################
      # Set up necessary data for the catR function
      ##############################################################################
  
      # this is for the out argument. 
      # creates a vector of the items that have already been completed
      # to be fed to IRT so they don't get chosen again
      completed = all_items[!is.na(all_items$response),]$item_number
      # completed = all_items %>% 
      #   tidyr::drop_na(response) %>%
      #   dplyr::pull(item_number)

      # don't re-use previous items
      if(exclude_previous){
        # previously_completed = previous %>%
        # # selects only done items and grabs them.
        #                         dplyr::pull(item_number)
        
        previously_completed = previous$item_number
        completed = c(completed, previously_completed)
      }

      ##############################################################################
      # catR functions for calculating ability and SEM go in here
      ##############################################################################
      # These will always be the same - takes in data of completed items and scores
      
      pars = data.frame(a = all_items$discrimination,
                        b = all_items$itemDifficulty, # CHANGE TO T SCORES 50 +/- 10
                        c = rep(1), #1PL has no guessing parameter ,
                        d = rep(0), #1PL has no innatention parameter,
                        cbGroup = rep(1))
      # breaks it down into what gets fed into the 1PL IRT
      prov = catR::breakBank(pars)
      bank = prov$itemPar
      rownames(bank) <- all_items$target
      x = all_items$response
       # ability estimate using bayes modal:
      # 10-6 CHANGING TO T ESTIMATES
       ability = catR::thetaEst(bank, x, method = "EAP", parInt = c(5, 95, 33), priorPar = c(50,10))
       # generates the next item
       # standard error of the mean
       # CHANGE FOR T-SCORE HERE
       sem = catR::semTheta(ability, bank, x, method = "EAP", parInt = c(5, 95, 33), priorPar = c(50,10))
       
       ##############################################################################
       # Choosing the next item however depends on the kind of test that we're doing
       ##############################################################################
       
       # If we're doing a computer adaptive test:
       if(IRT){
         completed = c(completed, 49) # removes eskimo from the item pool. 
         next_item = if(length(completed)<175){ # as long as we haven't done 175 items
           catR::nextItem(itemBank = bank, theta = ability, out = completed,
                          method = "EAP", range = c(5, 95), priorPar = c(50,10))
         } else {
           NA # returning NA will end the test?
         }
          # save ability, sem, and next item in a list, return the list, end the function. 
         tmp_list = list(
         ability,
         next_item,
         sem
         )
       return(tmp_list)
         
        # If we're doing one of the walker short-forms
       } else if(walker) {

         next_slide_num <- all_items
         next_slide_num$next_item = ifelse(!is.na(next_slide_num$response), next_slide_num$walker_order+1, NA)
         next_slide_num <- next_slide_num[next_slide_num$walker_order==max(next_slide_num$next_item, na.rm = T),]
         
         tmp_list = list(
           ability,
           # structured like this becasue the catR::nextItem function returns a list
           # where the second item in teh list is the next slide number
           list( 
             NA,
             slide_num_out = ifelse(nrow(next_slide_num) < 1, 190, next_slide_num$slide_num)
           ),
           sem
         )
         
         return(tmp_list)
         
      # IF we're doing the standard PNT
    } else { 
        if(exclude_eskimo){ # if excluding eskimo...
      
          # deal with eskimo here...
        next_slide_num <- all_items[all_items$item_number != 49 & is.na(all_items$response),]
            if(nrow(next_slide_num)>=1){
              next_slide_num <- next_slide_num[next_slide_num$pnt_order==min(next_slide_num$pnt_order),]
            }
        
        # helps with ending the test
        out_stop = 189
        
        } else { # if not excluding eskimo...

          next_slide_num <- all_items[is.na(all_items$response),]
            if(nrow(next_slide_num)>=1){
              next_slide_num <- next_slide_num[next_slide_num$pnt_order==min(next_slide_num$pnt_order),]
            }
        # helps with ending the test. see tmp list
        out_stop = 190
        }
      
      tmp_list = list(
        ability,
        list(
          NA,
          slide_num_out = ifelse(nrow(next_slide_num) < 1, out_stop, next_slide_num$slide_num)
          ),
        sem
      )
      
      return(tmp_list)
      
    }
}







