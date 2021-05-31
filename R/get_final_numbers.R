get_final_numbers <- function(out,
                              previous,
                              num_previous){
  df = tibble(
    ability = out[[1]],
    sem = out[[3]],
    last_ability = NA,
    last_sem = NA,
    first_ability = NA,
    first_sem = NA
  )
  
  if(!is.null(previous)){
    if(num_previous==1){
      df$last_ability = previous[previous$sem==min(previous$sem),]$ability
      df$last_sem = min(previous$sem)
    } else if (num_previous == 2){
      prev_dat = previous %>% filter(date == max(date))
      df$last_ability = prev_dat[prev_dat$sem==min(prev_dat$sem),]$ability
      df$last_sem = min(prev_dat$sem)
      first_dat = previous %>% filter(date == min(date))
      df$first_ability = first_dat[first_dat$sem==min(first_dat$sem),]$ability
      df$first_sem = min(first_dat$sem)
    }
  }
  return(df)
}
