

#' Compiles data for downloading
#'
#' @param values all the reactive values in the session
#' @param in_progress whether or not the test was ended early. 
#'
#' @return a dataframe of responses for downloading
#' @export
get_data_for_download <- function(values, in_progress){
  
  # if an uploaded test, just do this...
  if(isTruthy(values$score_uploaded_test)){
    return(score_uploaded_data(values = values)$data)
  }
  
  tmp <- get_results_data_long(values)

  tmp$test = values$selected_test
  

  # only add walker column for walker tests. otherwise just confusing. 
 if(grepl("walker", values$selected_test)){
    
    columns = c("item_number", "target", "key", "resp", "response", "discrimination",
                "itemDifficulty", "slide_num", "order", "pnt_order", "ability", "sem", "ci95_lower",
                "ci95_upper", "test", "walker", "walker_order", "start", "end", "notes")
    
  } else {
    
    columns = c("item_number", "target", "key", "resp", "response", "discrimination",
                "itemDifficulty", "slide_num", "order", "pnt_order", "ability", "sem", "ci95_lower",
                "ci95_upper", "test",
                # "walker", "walker_order",
                "start", "end", "notes")
   
  }

dat_out <- tmp[,columns]

if(in_progress == "Assessment" & isTruthy(values$IRT)){
    dat_out$notes[2] = "Test ended before completed"
    dat_out$notes[3] = "Next item"
    dat_out$notes[4] = values$irt_out[[2]]$name
}
return(dat_out)   
}