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
    last_ci_95 = NA,
    first_ability = NA,
    first_ci_95 = NA
  )
  
  if(!is.null(previous)){
    if(num_previous==1){
      df$last_ability = previous[previous$ci_95==min(previous$ci_95),]$ability
      df$last_ci_95 = min(previous$ci_95)
    } else if (num_previous == 2){
      prev_dat = previous %>% dplyr::filter(date == max(date))
      df$last_ability = prev_dat[prev_dat$ci_95==min(prev_dat$ci_95),]$ability
      df$last_ci_95 = min(prev_dat$ci_95)
      first_dat = previous %>% dplyr::filter(date == min(date))
      df$first_ability = first_dat[first_dat$ci_95==min(first_dat$ci_95),]$ability
      df$first_ci_95 = min(first_dat$ci_95)
    }
  }
  return(df)
}
