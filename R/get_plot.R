
#' the plot for the results page and rmarkdown documents
#'
#' generates a plot for the results pages and rmarkdown documents using the final scores
#' theta estimates from all of the current PNT norms (in sys.Data).
#' 
#' @param irt_final final irt estimates from get_final_numbers
#' @param basesize basesize for font of the plot. changes for rmarkdown
#' @param sample_thetas vector of PNT ability scores from MAPPD database and R03
#' 
#' @return a ggplot object
#' 
#' @export
get_plot <- function(irt_final, basesize = 18, sample_thetas = thetas){
  
  
  theta_df = tibble::tibble( thetas = c(thetas, 80, 20) )

  q = ggplot2::ggplot(data = theta_df,
    ggplot2::aes(x = thetas)
    ) + 
  ggplot2::geom_density(alpha = 0.4, fill = "lightgrey", adjust = 1.5)
  
  q_dat <- ggplot2::ggplot_build(q)
  x1 <- min(which(q_dat$data[[1]]$x >=irt_final$ability-(irt_final$sem*1.96)))
  x2 <- max(which(q_dat$data[[1]]$x <=irt_final$ability+(irt_final$sem*1.96)))
  
  y_dens = subset(q_dat$data[[1]], abs(x-irt_final$ability) == min(abs(x - irt_final$ability)))$y
        
  q <- q +
    ggplot2::geom_area(data=data.frame(x=q_dat$data[[1]]$x[x1:x2],
                              y=q_dat$data[[1]]$y[x1:x2]),
              ggplot2::aes(x=x, y=y), fill="blue3", alpha = 0.3) +

    ggplot2::geom_vline(ggplot2::aes(xintercept = irt_final$ability),
                        linetype = "dashed", color = "darkblue", size = 1.25) +
    ggplot2::labs(x = "PNT naming ability score") +
      ggplot2::theme_minimal(base_size = basesize) +
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
  
  if(!is.na(irt_final$last_ability)){
    
    x1b <- min(which(q_dat$data[[1]]$x >=irt_final$last_ability-(irt_final$last_sem*1.96)))
    x2b <- max(which(q_dat$data[[1]]$x <=irt_final$last_ability+(irt_final$last_sem*1.96)))
    
    
    q = q + 
      ggplot2::geom_area(data=data.frame(x=q_dat$data[[1]]$x[x1b:x2b],
                                         y=q_dat$data[[1]]$y[x1b:x2b]),
                         ggplot2::aes(x=x, y=y), fill="red3", alpha = 0.15) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = irt_final$last_ability),
                          linetype = "dashed", color = "darkred", size = 1.25)

  }

  return(q)
}


    