#' get data string
#'
#' @param dat dat
#' @param var var
#' @export
get_data_string <- function(dat, var){
  paste(dat %>%
          tidyr::drop_na(response) %>%
          dplyr::pull(var), collapse = "_")
}