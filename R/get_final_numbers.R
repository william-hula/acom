#' obtains final numbers from the testing data. 
#'
#' @param out list of the final numbers from the get_irt function
#' @param previous previous items
#' @param num_previous how many previous tests...0 or 1
#' 
#' @return a small dataframe of final number estimates
#' @export
get_final_numbers <- function(out,
                              previous,
                              num_previous){
  # df = data.frame(
  #   ability = out[[1]],
  #   sem = out[[3]],
  #   last_ability = NA,
  #   last_sem = NA
  # )
  # 
  # if(!is.null(previous)){
  #     df$last_ability = previous[previous$order==max(previous$order, na.rm = T),]$ability
  #     df$last_sem = previous[previous$order==max(previous$order, na.rm = T),]$sem
  #   }
  # 
  # return(df)
}
