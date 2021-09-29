
# note 95 CI / 2 is not a SD. Need to figure out actual conversion...

#' get plot
#'
#' @param values values
#' @param irt_final final irt
#' @export
    get_plot <- function(values, irt_final, basesize = 15){
      df <- tibble::tibble(
        ability = seq(-4, 4, .1),
        this_test = dnorm(seq(-4, 4, .1), irt_final$ability, irt_final$ci_95/2),
        last_test = if(!is.na(irt_final$last_ability)) {
          dnorm(seq(-4, 4, .1), irt_final$last_ability, irt_final$last_ci_95/2)
          } else {
            NA
          },
        first_test = if(!is.na(irt_final$first_ability)) {
          dnorm(seq(-4, 4, .1), mean = 1, sd = .6)
          } else {
            NA
          }
      ) %>% tidyr::pivot_longer(
        cols = 2:4,
        names_to = "time",
        values_to = "est"
      ) %>%
        tidyr::drop_na()
      
      
      p = df %>%
        ggplot2::ggplot(ggplot2::aes(x = ability, y = est, fill = time), color = "back") +
        ggplot2::geom_line(size = 1) +
        ggplot2::geom_polygon(alpha = .4) +
        ggplot2::theme_minimal(base_size = basesize) +
        ggplot2::theme(legend.position = "bottom",
              axis.title.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank()
        ) +
        ggplot2::labs(fill = "Test", x = "PNT Ability Estimate") +
        ggplot2::scale_x_continuous(minor_breaks =seq(-4,4,.5), limits = c(-4,4),
                           breaks = seq(-4,4,.5),
                           labels = seq(-4,4,.5))
      return(p)
    }
