#' Scores data that has been uploaded. 
#'
#' @return list of stuff needed for uploaded data
#' @export
score_uploaded_data <- function(uploaded_dat){
  to_join <- item_key %>% dplyr::select(target, itemDifficulty)
  dat <- uploaded_dat %>%
    dplyr::left_join(to_join, by = "target") %>%
    dplyr::mutate(resp = ifelse(key == 1, "incorrect",
                                ifelse(key == 2, 
                                       "correct", NA)),
                  response = ifelse(key == 1, 1,
                                    ifelse(key == 2, 
                                           0, NA)),
                  discrimination = 1.258
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
  ability = catR::thetaEst(bank, x, method = "EAP", range = c(-5, 5))
  # generates the next item
  # standard error of the mean
  sem = catR::semTheta(ability, bank, x)
  ci_95 = sem*1.96
  accuracy = sum(dat$key==2, na.rm = T)/(sum(dat$key==1, na.rm = T)+sum(dat$key==2, na.rm = T))
    
  out_list <- list()
  
  out_list$final_ability = ability
  out_list$final_sem = sem
  out_list$final_95_ci = ci_95
  out_list$final_accuracy = accuracy
  out_list$date_scored = Sys.Date()
  out_list$data = dat
  
  ####### TEXT ######
  
  out_list$text <- 
    paste(
      "The total accuracy for this test was ",
      round(accuracy*100, 1),
      "%. ",
      "The final IRT ability estimate is ",
      round(ability, 2),
      " [95% CI: ", round(ability - ci_95,2), ", ", round(ability + ci_95,2), "]. ",
      "This naming ability estimate is in the ",
      round(pnorm(ability, 0, 1.48)*100,1), " percentile of naming ability."
      ,sep = "")
  
  ###### PLOT ########
  
  out_list$plot <- tibble::tibble(
    ability = seq(-4, 4, .1),
    est = dnorm(seq(-4, 4, .1), out_list$final_ability, out_list$final_95_ci/2),
    timepoint = out_list$date_scored
  ) %>%
    ggplot2::ggplot(ggplot2::aes(x = ability, y = est, fill = timepoint),
                    color = "back") +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_polygon(alpha = .4) +
    ggplot2::theme_minimal(base_size = 15) +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "PNT Ability Estimate") +
    ggplot2::scale_x_continuous(minor_breaks =seq(-4,4,.5), limits = c(-4,4),
                                breaks = seq(-4,4,.5),
                                labels = seq(-4,4,.5))

  return(out_list)
  
}
