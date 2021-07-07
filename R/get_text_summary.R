  
get_text_summary <- function(
  acc,
  ability,
  sem,
  last_ability,
  last_sem,
  first_ability,
  first_sem,
  num_previous
){
    summary =  paste(
        "The total accuracy for this test was",
        round(acc*100, 1),
        "%.",
        "The final IRT ability estimate is",
        round(ability, 2),
        "and the standard error of the mean is",
        round(sem,2),
        "(blue).", "This naming ability estimate is in the ",
        round(pnorm(ability, 0, 1.48)*100,1), "percentile of naming ability."
        ,sep = " ")
    
    if(num_previous >= 1){
      summary = 
        paste(
          summary,
          "Last assessment, the final IRT ability estimate was ",
          round(last_ability,2),
          "and the standard error of the mean was",
          round(last_sem,2),
          "(red).", "The previous naming ability estimate was in the",
          round(pnorm(last_ability, 0, 1.48)*100,1), " percentile."
          ,sep = " ")
    }
      
      if(num_previous == 2){
        summary = 
          paste(
            summary,
            " In the first assessment, the final IRT ability estimate was ",
            round(first_ability,2),
            "and the standard error of the mean was",
            round(first_sem,2),
            "(green).", "The first naming ability estimate was in the",
            round(pnorm(first_ability, 0, 1.48)*100,1), "percentile."
            ,sep = " ")
        
      }
    
    return(p(summary))
  
}
