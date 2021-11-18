

#' Pulls a pasted column from the final data for exporting data from the app during testing. 
#'
#' @param dat dataframe of final results in
#' @param pull column to select from the final data frame
#'
#' @return a string of concatenated values with _ between them
pull_column <- function(dat, pull){
  
  pull_tmp = deparse(substitute(pull))
  # remove rows where no response  # just the column of interest
  dat = dat[!is.na(dat$response),pull_tmp]
  # paste together
  tmp = paste(dat, collapse = "_")

    return(tmp)
}



