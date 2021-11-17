
# note 95 CI / 2 is not a SD. Need to figure out actual conversion...

#' get plot
#'
#' @param irt_final final irt
#' @export
    get_plot <- function(irt_final, basesize = 18, sample_thetas = thetas){
      
      
      thetas = c(thetas, 80, 20)
      q = tibble::tibble(thetas) %>%
        ggplot2::ggplot(
          ggplot2::aes(x = thetas)
          ) + 
        #ggplot2::geom_histogram(alpha = 0.4, binwidth = 1) +
        ggplot2::geom_density(alpha = 0.4, fill = "lightgrey", adjust = 1.5)# +
        #ggplot2::geom_histogram(alpha = 0.2,
                                  #fill = "blue3", binwidth = 1,
                                  #data = subset_dat(tibble::tibble(thetas))) +
      
      q_dat <- ggplot2::ggplot_build(q)
      x1 <- min(which(q_dat$data[[1]]$x >=irt_final$ability-(irt_final$sem*1.96)))
      x2 <- max(which(q_dat$data[[1]]$x <=irt_final$ability+(irt_final$sem*1.96)))
      y_dens = q_dat$data[[1]] %>% 
        dplyr::filter(abs(x-irt_final$ability) == min(abs(x - irt_final$ability))) %>%
        dplyr::pull(y)
            
      q <- q +
        ggplot2::geom_area(data=data.frame(x=q_dat$data[[1]]$x[x1:x2],
                                  y=q_dat$data[[1]]$y[x1:x2]),
                  ggplot2::aes(x=x, y=y), fill="blue3", alpha = 0.3) +
      
        # ggplot2::geom_segment(ggplot2::aes(x=irt_final$ability,
        #                                    y = 0,
        #                                    xend=irt_final$ability,
        #                                    yend = y_dens),#25),
        #                       color = "darkblue", size = 1.25) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = irt_final$ability), linetype = "dashed", color = "darkblue", size = 1.25) +
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
        y_densb = q_dat$data[[1]] %>% 
          dplyr::filter(abs(x-irt_final$last_ability) == min(abs(x - irt_final$last_ability))) %>%
          dplyr::pull(y)
        
        q = q + 
          ggplot2::geom_area(data=data.frame(x=q_dat$data[[1]]$x[x1b:x2b],
                                             y=q_dat$data[[1]]$y[x1b:x2b]),
                             ggplot2::aes(x=x, y=y), fill="red3", alpha = 0.15) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = irt_final$last_ability), linetype = "dashed", color = "darkred", size = 1.25)
          # ggplot2::geom_segment(ggplot2::aes(x=irt_final$last_ability,
          #                                    y = 0,
          #                                    xend=irt_final$last_ability,
          #                                    yend = y_densb),#25),
          #                       color = "darkred", size = 1.25)
        
        
      }
      # This will need to be changed to accomodate new T distribution. 
       

      
      
      
      return(q)
    }

    
    
    # df <- tibble::tibble(
    #   ability = seq(5, 95, .5),
    #   this_test = dnorm(seq(5, 95, .5), irt_final$ability, irt_final$ci_95/2),
    #   last_test = if(!is.na(irt_final$last_ability)) {
    #     dnorm(seqseq(5, 95, .5), irt_final$last_ability, irt_final$last_ci_95/2)
    #     } else {
    #       NA
    #     },
    #   first_test = if(!is.na(irt_final$first_ability)) {
    #     NA #dnorm(seqseq(5, 95, .5), mean = 1, sd = .6)
    #     } else {
    #       NA
    #     }
    # ) %>% tidyr::pivot_longer(
    #   cols = 2:4,
    #   names_to = "time",
    #   values_to = "est"
    # ) %>%
    #   tidyr::drop_na()
    # 
    # print(df) 
    # 
    # p = df %>%
    #   ggplot2::ggplot(ggplot2::aes(x = ability, y = est, fill = time), color = "black") +
    #   #ggplot2::geom_line(size = 1) +
    #   ggplot2::geom_polygon(alpha = .4) +
    #   ggplot2::theme_minimal(base_size = basesize) +
    #   ggplot2::theme(legend.position = "bottom",
    #         axis.title.y = ggplot2::element_blank(),
    #         axis.text.y = ggplot2::element_blank(),
    #         axis.ticks.y = ggplot2::element_blank()
    #   ) +
    #   ggplot2::labs(fill = "Test", x = "PNT Ability Estimate") #+
    #   # This will need to be changed to accomodate new T distribution. 
    #   # ggplot2::scale_x_continuous(minor_breaks =seq(-4,4,.5), limits = c(-4,4),
    #   #                    breaks = seq(-4,4,.5),
    #   #                    labels = seq(-4,4,.5))
    # p = p + ggplot2::geom_histogram(data = tibble::tibble(thetas), ggplot2::aes(x = thetas, y=..density..), alpha = 0.4, inherit.aes = F)
    # 
    # 
    # subset_dat <- function(dat){
    #   lower = irt_final$ability-irt_final$ci_95/1.96
    #   upper = irt_final$ability+irt_final$ci_95/1.96
    #   subset(dat,thetas>lower&thetas<upper)
    # }