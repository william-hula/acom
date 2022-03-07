



#' Compiles final results in long format
#' 
#' Also appends notes, and other test information to the final dataframe
#'
#' @param values list of stuff from server
#' @return a dataframe of final result and test information
#' @export
get_results_data_long <- function(values){
  
  req(isTruthy(values$downloadableData))
  precision = if(values$test_length == "SEM"){
    paste0("Based on first test")
  } else {
    paste0(values$test_length, " items")
  }
  
  tmp = rbind(values$item_difficulty)
  
  tmp$ci95_lower = tmp$ability - tmp$sem*1.96
  tmp$ci95_upper = tmp$ability + tmp$sem*1.96
  tmp$precision = precision
  tmp$start = values$start_time
  tmp$end = values$end_time
  tmp$notes = NA
  
  tmp = tmp[order(tmp$order), , drop = FALSE]
  
  tmp$notes[[1]] = ifelse(values$new_test, values$notes, values$notes_retest)
  return(tmp)
  
}