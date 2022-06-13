#' Scores data that has been uploaded. 
#'
#' @return a list containing final IRT estimates and a text summary via get_text_summary()
#' @export
score_uploaded_data <- function(values){
  
  # only need to do this if uploaded a blank csv. 
  if(ncol(values$rescore)<5){
  to_join <- items[,c("target", "itemDifficulty", "discrimination")] 
  
  dat <- merge(values$rescore, to_join, by = "target")
  dat$resp = ifelse(dat$key == 1, "incorrect",
                    ifelse(dat$key == 2, 
                           "correct", NA))
  dat$response = ifelse(dat$key == 1, 1,
                        ifelse(dat$key == 2, 
                               0, NA))
  } else {
    # 
    dat = values$rescore
    dat$resp = ifelse(dat$key == 1, "incorrect",
                      ifelse(dat$key == 2, 
                             "correct", NA))
    dat$response = ifelse(dat$key == 1, 1,
                          ifelse(dat$key == 2, 
                                 0, NA))
  }
  
  pars = data.frame(a = dat$discrimination,
                    b = dat$itemDifficulty,
                    c = rep(0), #1PL has no guessing parameter ,
                    d = rep(1), #1PL has no innatention parameter,
                    cbGroup = rep(1))
  # breaks it down into what gets fed into the 1PL IRT
  prov = catR::breakBank(pars)
  bank = prov$itemPar
  rownames(bank) <- dat$target
  x = dat$response
  # ability estimate using bayes modal:
  ability = catR::thetaEst(bank, x,
                           method = irt_params$method,
                           parInt = irt_params$parInt,
                           priorPar = irt_params$priorPar)
  # generates the next item
  # standard error of the mean
  sem = catR::semTheta(ability, bank, x,
                       method = irt_params$method,
                       parInt = irt_params$parInt,
                       priorPar = irt_params$priorPar)
  ci_95 = sem*1.96
  accuracy = sum(dat$key==2, na.rm = T)/(sum(dat$key==1, na.rm = T)+sum(dat$key==2, na.rm = T))
    
  out_list <- list()
  
  out_list$irt_final = tibble::tibble(
    ability = ability,
    sem = sem,
    last_ability = NA,
    last_sem = NA
  )

  out_list$final_accuracy = accuracy
  out_list$date_scored = Sys.Date()
  
  dat$ability = round(ability,4)
  dat$sem = round(sem,4)
  dat$ci_95 = round(ci_95,4)
  if(ncol(values$rescore)<5){
  dat$order = NA
  dat$name = NA
  dat$notes = NA
  dat$date = Sys.Date()
  dat$test = "Offline test"
  }
  out_list$data = dat
  ####### TEXT ######
  
  out_list$rescored_items = sum(!is.na(dat$response))
  
  return(out_list)
  
}
