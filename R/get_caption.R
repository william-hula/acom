


#' The caption for the plot. 
#' 
#' It changes based on whether its a repeat administration or not. 
#'
#' @param values all values
#'
#' @return a html tag with the caption in italics
#' @export
get_caption <- function(repeat_admin){
  
  # if(sum(!is.na(values$item_difficulty$response))<30){
  #   return("Please administer more items to obtain a reliable ability estimate. If desired, results can be downloaded above,
  #          and the current test continued by re-uploading this data.")
  # }
  
  if(!isTruthy(repeat_admin)){
    
   cap = "The blue dashed line reflects current estimate and the shaded area reflects uncertainty in current estimate. The average ability for individuals with aphasia is 50, with a standard deviation of 10. The density plot represents the distribution of ability scores in the calibration sample, which includes 296 cases from the Moss Aphasia Psycholinguistic Project Database and 47 cases from NIH/NIDCD Award R03DC014556 (PI: Fergadiotis)"
    
  } else {
    
   cap = "The blue dashed line reflects current estimate and the red dashed line reflects the estimate from the previous test. Shaded areas reflects uncertainty in the estiates. The average ability for individuals with aphasia is 50, with a standard deviation of 10. The density plot represents the distribution of ability scores in the calibration sample, which includes 296 cases from the Moss Aphasia Psycholinguistic Project Database and 47 cases from NIH/NIDCD Award R03DC014556 (PI: Fergadiotis)"
  }
  #cat("\n",cap)
  return(cap)
}