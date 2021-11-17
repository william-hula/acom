#' final numbers
#'
#' @param out out
#' @param previous prev
#' @param num_previous num prev
#' @export
get_final_numbers <- function(out,
                              previous,
                              num_previous){
  df = tibble::tibble(
    ability = out[[1]],
    sem = out[[3]],
    ci_95 = out[[3]]*1.96,
    last_ability = NA,
    last_sem = NA,
    last_ci_95 = NA
  )
  
  if(!is.null(previous)){
      df$last_ability = previous[previous$order==max(previous$order, na.rm = T),]$ability
      df$last_sem = previous[previous$order==max(previous$order, na.rm = T),]$sem
      df$last_ci_95 = previous[previous$order==max(previous$order, na.rm = T),]$ci_95
    }

  return(df)
}
