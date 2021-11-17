#' Scores data that has been uploaded. 
#'
#' @return list of stuff needed for uploaded data
#' @export
score_uploaded_data <- function(uploaded_dat){
  to_join <- items %>% dplyr::select(target, itemDifficulty, discrimination)
  dat <- uploaded_dat %>%
    #dplyr::rename(key = response) %>%
    dplyr::left_join(to_join, by = "target") %>%
    dplyr::mutate(resp = ifelse(key == 1, "incorrect",
                                ifelse(key == 2, 
                                       "correct", NA)),
                  response = ifelse(key == 1, 1,
                                    ifelse(key == 2, 
                                           0, NA))
    )
  
  pars = data.frame(a = dat$discrimination,
                    b = dat$itemDifficulty,
                    c = rep(1), #1PL has no guessing parameter ,
                    d = rep(0), #1PL has no innatention parameter,
                    cbGroup = rep(1))
  
  # breaks it down into what gets fed into the 1PL IRT
  prov = catR::breakBank(pars)
  bank = prov$itemPar
  rownames(bank) <- dat$target
  x = dat$response
  # ability estimate using bayes modal:
  ability = catR::thetaEst(bank, x, method = "EAP", parInt = c(5, 95, 33), priorPar = c(50,10))
  # generates the next item
  # standard error of the mean
  sem = catR::semTheta(ability, bank, x, method = "EAP", parInt = c(5, 95, 33), priorPar = c(50,10))
  ci_95 = sem*1.96
  accuracy = sum(dat$key==2, na.rm = T)/(sum(dat$key==1, na.rm = T)+sum(dat$key==2, na.rm = T))
    
  out_list <- list()
  
  out_list$irt_final = tibble::tibble(
    ability = ability,
    sem = sem,
    ci_95 = ci_95,
    last_ability = NA
  )
  
  # out_list$final_ability = ability
  # out_list$final_sem = sem
  # out_list$final_95_ci = ci_95
  out_list$final_accuracy = accuracy
  out_list$date_scored = Sys.Date()
  out_list$data = dat
  
  ####### TEXT ######
  
  out_list$text <- get_text_summary(
    ability = ability,
    sem = sem,
    last_ability = NA,
    last_sem = NA,
    num_previous = 0
  )

  return(out_list)
  
}
