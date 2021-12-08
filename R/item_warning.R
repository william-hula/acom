

#' Item Warning
#' returns a warning if not enough items are administered
#'
#' @return a warning div
#' @export
get_item_warning <- function(values){
  
  check = nrow(values$results_data_long[!is.na(values$results_data_long$key),])
  #print(head(values$results_data_long[!is.na(values$results_data_long$key),], 31))
  cat("Item warning check: the number of items administered is", check, "\n")
  
  if(values$selected_test == "SEM"){
    VL = T
  } else {
    VL = F
  }
  
  if(!isTruthy(VL) & check < 30){
    out = tags$em("Warning: Less than 30 items have been administered. Naming ability estimates may not be reliable. ",
        style = "color:darkred;"
    )
    return(out)
  } else {
    return("")
  }
  
  
  
}

