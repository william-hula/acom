

#' gets data for download. 
#'
#' @param dat 
#'
#' @return a dataframe of responses
#' @export
get_data_for_download <- function(dat, in_progress, current_item, IRT = T){
dat_out <- dat %>% dplyr::select(item_number, target, key, resp,
                          order, ability, sem, ci_95,
                          name, date, notes)

if(in_progress == "Assessment" & isTruthy(IRT)){
    dat_out$notes[2] = "Test ended before completed"
    dat_out$notes[3] = "Next item"
    dat_out$notes[4] = current_item
}
return(dat_out)   

}