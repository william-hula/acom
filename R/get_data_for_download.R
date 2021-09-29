

#' gets data for download. 
#'
#' @param dat 
#'
#' @return a dataframe of responses
#' @export
get_data_for_download <- function(dat){
dat_out <- dat %>% dplyr::select(item_number, target, key, resp,
                          order, ability, sem, ci_95,
                          name, date, notes)
return(dat_out)   

}