

#' Compiles data for downloading
#'
#' @param values all the reactive values in the session
#' @param in_progress whether or not the test was ended early. 
#'
#' @return a dataframe of responses for downloading
#' @export
get_data_for_download <- function(values, in_progress){
  
  current_item = values$irt_out[[2]]$name
  IRT = values$IRT
  
  tmp <- get_results_data_long(values)
  
  test = ifelse(grepl("walker", values$selected_test), paste0(values$selected_test, "_", values$walker_form), values$selected_test)

tmp$test = test

dat_out <- tmp[,c("item_number", "target", "key", "resp", "response", "discrimination",
                  "itemDifficulty", "slide_num", "order", "ability", "sem", "ci_95", "test",
                  "name", "date", "notes")]

if(in_progress == "Assessment" & isTruthy(IRT)){
    dat_out$notes[2] = "Test ended before completed"
    dat_out$notes[3] = "Next item"
    dat_out$notes[4] = current_item
}
return(dat_out)   

}