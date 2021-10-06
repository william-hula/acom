
# note 95 CI / 2 is not a SD. Need to figure out actual conversion...

#' get plot
#'
#' @param values values
#' @param irt_final final irt
#' @export
    get_plot <- function(values, irt_final, basesize = 18, sample_thetas = thetas){
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
      subset_dat <- function(dat){
        lower = irt_final$ability-irt_final$ci_95/2
        upper = irt_final$ability+irt_final$ci_95/2
        subset(dat,thetas>lower&thetas<upper)
      }
      
      q = tibble::tibble(thetas) %>%
        ggplot2::ggplot(
          ggplot2::aes(x = thetas)
          ) + 
        ggplot2::geom_histogram(alpha = 0.25, binwidth = 1) +
        #ggplot2::geom_histogram(alpha = 0.2, fill = "blue3", binwidth = 1, data = subset_dat(tibble::tibble(thetas))) +
        ggplot2::annotate("rect",
                          xmin = irt_final$ability-irt_final$ci_95/2,
                          xmax = irt_final$ability+irt_final$ci_95/2,
                          ymin = 0,
                          ymax = 25,
                          alpha = .1,
                          fill = "blue3") +
        ggplot2::geom_segment(ggplot2::aes(x=irt_final$ability,
                                           y = 0,
                                           xend=irt_final$ability,
                                           yend = 25), color = "darkred", size = 1.25) +
        ggplot2::labs(x = "PNT naming ability score",
                      caption = "Red line reflects current estimate. Shaded area reflects uncertainty in current estiate.\n The average ability for individuals with aphasia is 50, with a standard deviation of 10.") +
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
      # This will need to be changed to accomodate new T distribution. 
       
      
      
      
      
      return(q)
    }
