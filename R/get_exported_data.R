get_data_string <- function(dat, var){
  paste(dat %>%
          drop_na(response) %>%
          pull(var), collapse = "_")
}