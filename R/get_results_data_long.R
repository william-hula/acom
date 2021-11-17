



#' Title Get Results data long
#'
#' @param values list of stuff from server
#'
#' @export
get_results_data_long <- function(values){
  
  req(isTruthy(values$downloadableData))
  precision = if(values$test_length == "SEM"){
    paste0("Based on first test")
  } else {
    paste0(values$test_length, " items")
  }
  
  tmp = dplyr::bind_rows(values$item_difficulty) %>%
    dplyr::mutate(ci_95 = sem*1.96,
                  precision = precision,
                  name = values$name,
                  date = values$datetime,
                  notes = NA
    ) %>%
    dplyr::arrange(order)
  
  tmp$notes[[1]] = ifelse(values$new_test, values$notes, values$notes_retest)
  return(tmp)
  
}