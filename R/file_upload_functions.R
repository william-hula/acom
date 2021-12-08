

#' Upload previous data
#'
#' @param file_input 
#'
#' @return a dataframe of previous scores, or an empty df if not validated. 
#' @export
uploadData <- function(file_input, rescore = F){

  file <- file_input
  # check upload
  req(file)
  # save upload
  dat <- read.csv(file$datapath) 
  
  if(ncol(dat)<5){
    cols = c("item_number", "target", "key", "test")
  } else {
    cols = colnames(dat)
  }

  # no error to begin with
  error = NA
  
  # need these columns at minimum
  if (all(cols %in% colnames(dat))) {
        # remove NA rows
        dat = dat[!is.na(dat$key),cols]
        # need at least one response
        if(nrow(dat)==0){
          error = "Error: Please include at least one scored response"
        } else if (!all(unique(dat$key) == 1 | unique(dat$key) == 2)){
          error = "Error: please only enter 1 for correct and 2 for incorrect in the response column"
        } else if (!all(dat$target %in% item_key$target)){
        # target names must match
        error = "Error: some item names have changed"
        }
    } else {
    # the right columns aren't here. 
    error = "Error: Column names have been changed"
    dat <- tibble::tibble()
  }
  
  print(paste("Upload error:", error))

  if(is.na(error)){
    return(list(
      dat = dat,
      error = error
    ))
  }

}


