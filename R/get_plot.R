
# note 95 CI / 2 is not a SD. Need to figure out actual conversion...

    get_plot <- function(values, irt_final){
      df <- tibble(
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
      ) %>% pivot_longer(
        cols = 2:4,
        names_to = "time",
        values_to = "est"
      ) %>%
        drop_na()
      
      
      p = df %>%
        ggplot2::ggplot(aes(x = ability, y = est, fill = time), color = "back") +
        geom_line(size = 1) +
        geom_polygon(alpha = .4) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "bottom",
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()
        ) +
        labs(fill = "Test", x = "PNT Ability Estimate") +
        scale_x_continuous(minor_breaks =seq(-4,4,.5), limits = c(-4,4),
                           breaks = seq(-4,4,.5),
                           labels = seq(-4,4,.5))
      return(p)
    }

# get_plot <- function(values, irt_final){
#   
#   dens = density(bayestestR::distribution_normal(1000, 0, 1.48))
#   df <- tibble(
#     x = dens$x,
#     y = dens$y,
#     lower = irt_final$ability - irt_final$ci_95,
#     upper = irt_final$ability + irt_final$ci_95,
#     last_lower = ifelse(is.na(irt_final$last_ability), 
#                         1000,
#                         irt_final$last_ability - irt_final$last_ci_95),
#     last_upper = ifelse(is.na(irt_final$last_ability), 
#                         1001,
#                         irt_final$last_ability + irt_final$last_ci_95),
#     first_lower = ifelse(is.na(irt_final$first_ability), 
#                          1000,
#                          irt_final$first_ability - irt_final$first_ci_95),
#     first_upper = ifelse(is.na(irt_final$first_ability), 
#                          1001,
#                          irt_final$first_ability + irt_final$first_ci_95)
#   ) %>%
#     rowwise() %>%
#     mutate(fill1 = factor(ifelse(between(x, lower, upper),
#                                  "current", NA)),
#            fill2 = factor(ifelse(between(x, last_lower, last_upper),
#                                  "last", NA)),
#            fill3 = factor(ifelse(!between(x, lower, upper) & !between(x, last_lower, last_upper) & !between(x, first_lower, first_upper),
#                                  "neither", NA)),
#            fill4 = factor(ifelse(between(x, first_lower, first_upper),
#                                  "first", NA))
#     )
#   
#   p = df %>%
#     ggplot2::ggplot(aes(x = x, y = y)) +
#     geom_area(data = df %>% filter(fill3 == "neither"),
#               aes(y = y),position = "identity", fill = "#F1F1F1", color = NA) +
#     geom_area(data = df %>% filter(fill1 == "current"),
#               aes(y = y),position = "identity", fill = "#619CFF", alpha = .4, color = NA) +
#     
#     geom_line(size = 2) +
#     geom_vline(aes(xintercept = irt_final$ability), color = "#619CFF", alpha = .8, size = 1) +
#     scale_x_continuous(breaks=seq(-5,5,.5), limits = c(-5,5)) +
#     scale_fill_manual(guide = "none", values = colors) +
#     theme_minimal(base_size = 15) +
#     xlab("PNT Ability Estimate") +
#     ylab(NULL) +
#     theme(axis.title.x = element_text(vjust=-1),
#           plot.margin = unit(c(15, 5.5, 15, 5.5), "pt"),
#           legend.position = "none",
#           panel.grid = element_blank(),
#           axis.title.y = element_blank(),
#           axis.text.y = element_blank(),
#           axis.ticks.y = element_blank()) 
#   
#   if (values$num_previous>=1){
#     p = p + 
#       geom_area(data = df %>% filter(fill2 == "last"),
#                 aes(y = y),position = "identity", fill = "#F8766D", alpha = .4, color = NA) +
#       geom_vline(aes(xintercept = irt_final$last_ability), color = "#F8766D", alpha = .8, size = 1)
#   }
#   
#   if(values$num_previous == 2){
#     p = p + 
#       geom_area(data = df %>% filter(fill4 == "first"),
#                 aes(y = y),position = "identity", fill = "#00BA38", alpha = .4, color = NA) +
#       geom_vline(aes(xintercept = irt_final$first_ability), color = "#00BA38", alpha = .8, size = 1)
#   }
#   
#   
#   
#   return(p)
#   
#   
# }