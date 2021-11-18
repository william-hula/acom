

#' Title Pull Column
#'
#' @param dat dataframe in
#' @param pull column to select
#'
#' @return
pull_column <- function(dat, pull){
  
  pull_tmp = deparse(substitute(pull))
  # remove rows where no response  # just the column of interest
  dat = dat[!is.na(dat$response),pull_tmp]
  # paste together
  tmp = paste(dat, collapse = "_")

    return(tmp)
}



