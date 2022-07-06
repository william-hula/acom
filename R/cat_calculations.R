#' goCat
#'
#' @param v all values
#'
#' @return list of three irt parameters
#' @export
#'
goCAT <- function(v){
  
  dat = v$results %>% tidyr::drop_na(response) 
  
  done = dat$itnum
  
  valid_responses = nrow(dat %>% tidyr::drop_na(response_num))
  
  if(all(is.na(dat$response_num))){
    cur_theta = NA
    cur_sem = NA
    theta = 50
  } else {
    
    #estimate theta and sem
    cur_theta <- catR::thetaEst(it = dat[,4:7], x = dat$response_merge,
                          method = "EAP", model = "GRM",
                          D = 1, priorDist = "norm",
                          priorPar = c(50, 10),
                          parInt = c(10, 90, 33))
    
    theta = cur_theta
    
    cur_sem <- catR::semTheta(cur_theta, it = dat[,4:7], x = dat$response_merge,
                        method = "EAP", model = "GRM", D = 1, priorDist = "norm",
                        priorPar = c(50, 10), parInt = c(10, 90, 33))
    
  }
  
  # 
  if(length(valid_responses) < v$test_length){
  it_next <- catR::nextItem(itemBank = bank,
                      model = "GRM",
                      theta = theta,
                      out = done,
                      x = NULL,
                      criterion = "MFI",
                      method = "EAP",
                      priorDist = "norm", priorPar = c(50, 10),
                      D = 1, range = c(10, 90), parInt = c(10, 90, 33),
                      infoType = "observed",
                      randomesque = 1, random.seed = NULL,
                      rule = "length", thr = 20, SETH = NULL,
                      AP = 1, nAvailable = NULL, maxItems = 59, 
                      cbControl = acom_cbControl, cbGroup = acom_cb_group)
  
  next_item = it_next$item
  } else {
    next_item = NA
  }
  
  data_out = list(theta = cur_theta,
                  sem = cur_sem,
                  next_item = next_item)
  
  return(data_out)
  
}


#' response_to_numeric
#'
#' @param select selected response
#' @param clarify ask for clarification for doesn't apply
#'
#' @return numeric response
#' @export
response_to_numeric <- function(select, clarify, merge){
  # cannot do because of my communication problem = 0
  # cannot do for some other reason = NA
  # not very = 0
  # somewhat = 1
  # mostly = 2
  # completely = 3 
  
  df = tibble::tibble(response = select, clarify = clarify, merge_cats = merge) 
  #print(df)
  
  df = df %>%
    dplyr::mutate(response_num = dplyr::case_when(
      response == "Completely" ~ 3,
      response == "Mostly" ~ 2,
      response == "Somewhat" ~ 1,
      response == "Not very"~ 0,
      response == "Doesn't apply to me" ~ 0
    )) %>%
    dplyr::mutate(response_num = dplyr::case_when(
      response_num == 0 & clarify == "no" ~ NA_real_,
      response_num == 0 & clarify == "yes" ~ 0,
      TRUE ~ response_num
    )) %>%
    dplyr::mutate(response_merge = dplyr::case_when(
      merge_cats == 1 & response_num == 1 ~ 0,
      merge_cats == 1 & response_num == 2 ~ 1,
      merge_cats == 1 & response_num == 3 ~ 2,
      
      merge_cats == 2 & response_num == 2 ~ 1,
      merge_cats == 2 & response_num == 3 ~ 2,
      
      merge_cats == 3 & response_num == 3 ~ 2,
      
      merge_cats == 4 & response_num == 2 ~ 1,
      merge_cats == 4 & response_num == 3 ~ 1,
      TRUE ~ response_num
    )) %>%
    dplyr::select(response, response_num, response_merge, clarify)
  
  return(df)
}


#' Get text
#'
#' @param v all data
#'
#' @return the sentence of the current stimuli
#' @export

getTxt <- function(v) {
  # cat(paste0("Item number: ", v$itnum, "\n"))
  txt = d$item_content[v$itnum]
  return(txt)
}

