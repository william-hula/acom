

#' Compiles data for downloading
#'
#' @param values all the reactive values in the session
#' @param in_progress whether or not the test was ended early. 
#'
#' @return a dataframe of responses for downloading
#' @export
get_data_for_download <- function(values, in_progress){
  
  # if the test is in progress, and has acurrent response, log it: 
  if(in_progress == "Assessment" & isTruthy(values$key_val)){
    
    values$item_difficulty[values$item_difficulty$slide_num==values$n,]$response <-
      ifelse(values$key_val == incorrect_key_response, 1,
             ifelse(values$key_val == correct_key_response, 0, "NR"))
    
    values$irt_out = irt_function(all_items = values$item_difficulty,
                                  IRT = values$IRT,
                                  exclude_previous = values$exclude_previous,
                                  previous = values$previous,
                                  #test = input$numitems,
                                  exclude_eskimo = values$eskimo,
                                  walker = values$walker
    )
    # save info to the item_difficulty data_frame
    values$item_difficulty[values$item_difficulty$slide_num == values$n,]$order = values$i
    values$item_difficulty[values$item_difficulty$slide_num == values$n,]$key = values$key_val
    values$item_difficulty[values$item_difficulty$slide_num == values$n,]$resp = ifelse(values$key_val == incorrect_key_response,
                                                                                        "incorrect",
                                                                                        ifelse(values$key_val == correct_key_response,
                                                                                               "correct", "NR")
    )
    values$item_difficulty[values$item_difficulty$slide_num == values$n,]$ability = round(values$irt_out[[1]],4)
    values$item_difficulty[values$item_difficulty$slide_num == values$n,]$sem = round(values$irt_out[[3]],4)
    

  }
  
  tmp <- get_results_data_long(values)
  tmp$test = values$selected_test

  # only add walker column for walker tests. otherwise just confusing. 
 if(grepl("walker", values$selected_test)){
    
    columns = c("item_number", "target", "key", "resp", "response", "discrimination",
                "itemDifficulty", "slide_num", "order", "ability", "sem", "ci_95", "test",
                "walker", "walker_order",
                "name", "date", "notes")
    
  } else {
    
    columns = c("item_number", "target", "key", "resp", "response", "discrimination",
                "itemDifficulty", "slide_num", "order", "ability", "sem", "ci_95", "test",
                # "walker", "walker_order",
                "name", "date", "notes")
   
  }

dat_out <- tmp[,columns]

if(in_progress == "Assessment" & isTruthy(values$IRT)){
    dat_out$notes[2] = "Test ended before completed"
    dat_out$notes[3] = "Next item"
    dat_out$notes[4] = values$irt_out[[2]]$name
}
print(head(dat_out))
return(dat_out)   
}