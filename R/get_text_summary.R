  
#' get text summary
#'
#' @param acc acc
#' @param ability ability
#' @param ci_95 ci95
#' @param last_ability last abil
#' @param last_ci_95 last ci95
#' @param first_ability last first
#' @param first_ci_95 first ci
#' @param num_previous num prev
#' @export
get_text_summary <- function(
  acc,
  ability,
  ci_95,
  last_ability,
  last_ci_95,
  first_ability,
  first_ci_95,
  num_previous
){
    summary =  paste(
        "The total accuracy for this test was ",
        round(acc*100, 1),
        "%. ",
        "The final IRT ability estimate is ",
        round(ability, 2),
        " [95% CI: ", round(ability - ci_95/1.96,2), ", ", round(ability + ci_95/1.96,2), "]. ",
        "This naming ability estimate is in the ",
        round(pnorm(ability, 50, 10)*100,1), " percentile of naming ability."
        ,sep = "")
    
    if(num_previous >= 1){
      summary = 
        paste(
          summary,
          "Last assessment, the final IRT ability estimate was ",
          round(last_ability,2),
          " [95% CI: ", round(ability - last_ci_95/1.96,2), ", ", round(ability + last_ci_95/1.96,2), "]. ",
          "The naming ability estimate was in the ",
          round(pnorm(last_ability, 50, 10)*100,1), " percentile."
          ,sep = "")
    }
      
      if(num_previous == 2){
        summary = 
          paste(
            summary,
            " In the first assessment, the final IRT ability estimate was ",
            round(first_ability,2),
            " [95% CI: ", round(ability - last_ci_95/1.96,2), ", ", round(ability + last_ci_95/1.96,2), "]. ",
             "The first naming ability estimate was in the ",
            round(pnorm(first_ability, 50, 10)*100,1), " percentile."
            ,sep = "")
        
      }
    
    return((summary))
  
}
