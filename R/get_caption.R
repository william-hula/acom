


#' Get caption
#'
#' @param values all values
#'
#' @return
#' @export
get_caption <- function(values){
  if(is.na(values$irt_final$last_ability)){
    
   cap = tags$em("The blue dashed line reflects current estimate and the shaded area reflects uncertainty in current estiate. The average ability for individuals with aphasia is 50, with a standard deviation of 10. The density plot represents the distribution of ability scores in the calibration sample, which includes 296 cases from the Moss Aphasia Psycholinguistic Project Database and XX cases from NIH/NIDCD Award R03DC014556 (PI: Fergadiotis)")
    
  } else {
    
   cap =  tags$em("The blue dashed line reflects current estimate and the red dashed line reflects the estimate from the previous test. Shaded areas reflects uncertainty in the estiates. The average ability for individuals with aphasia is 50, with a standard deviation of 10. The density plot represents the distribution of ability scores in the calibration sample, which includes 296 cases from the Moss Aphasia Psycholinguistic Project Database and XX cases from NIH/NIDCD Award R03DC014556 (PI: Fergadiotis)")
  }
  
  return(cap)
}