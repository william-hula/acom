#' Scores data that has been uploaded. 
#'
#' @return list of stuff needed for uploaded data
#' @export
score_uploaded_data <- function(uploaded_dat){
  to_join <- items %>% dplyr::select(target, itemDifficulty, discrimination)
  dat <- uploaded_dat %>%
    dplyr::rename(key = response) %>%
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
      round(pnorm(ability, 50, 10)*100,1), " percentile of naming ability."
      ,sep = "")
  
  ###### PLOT ########
  
  # 
  # subset_dat <- function(dat){
  #   lower = irt_final$ability-irt_final$ci_95/1.96
  #   upper = irt_final$ability+irt_final$ci_95/1.96
  #   subset(dat,thetas>lower&thetas<upper)
  # }
  
  out_list$plot <- tibble::tibble(thetas) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = thetas)
    ) + 
    ggplot2::geom_histogram(alpha = 0.4, binwidth = 1) +
    #ggplot2::geom_histogram(alpha = 0.2, fill = "blue3", binwidth = 1, data = subset_dat(tibble::tibble(thetas))) +
    ggplot2::annotate("rect",
                      xmin = ability-ci_95/1.96,
                      xmax = ability+ci_95/1.96,
                      ymin = 0,
                      ymax = 25,
                      alpha = .15,
                      fill = "blue3") +
    ggplot2::geom_segment(ggplot2::aes(x=ability,
                                       y = 0,
                                       xend=ability,
                                       yend = 25), color = "darkred", size = 1.25) +
    ggplot2::labs(x = "PNT naming ability score",
                  caption = "Red line reflects current estimate. Shaded area reflects uncertainty in current estiate.\n The average ability for individuals with aphasia is 50, with a standard deviation of 10.") +
    ggplot2::theme_minimal(base_size = 18) +
    ggplot2::theme(legend.position = "bottom",
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::scale_x_continuous(
      minor_breaks =seq(10,90,5),
      limits = c(10, 90),
      breaks = seq(10,90,10),
      labels = seq(10,90,10)
    ) +
    ggplot2::scale_y_continuous(
      minor_breaks = NULL#,
      #breaks = NULL
    )

  return(out_list)
  
}
