get_exported_data <- function(results){
  # this is for making data available for export during testing.
  if (isTRUE(getOption("shiny.testmode"))) {  
    values$out_words <- paste(results %>% drop_na(response) %>%pull(target), collapse = "_")
    values$out_nums <- paste(results %>% drop_na(response) %>%pull(response), collapse = "_")
    values$out_ability <- paste(results %>% drop_na(response) %>%pull(ability), collapse = "_")
    values$out_sem <- paste(results %>% drop_na(response) %>%pull(sem), collapse = "_")
    values$item_dif <- paste(results %>% drop_na(response) %>%pull(itemDifficulty), collapse = "_")
    values$disc <- paste(results %>% drop_na(response) %>%pull(discrimination), collapse = "_")
    values$key <- paste(results %>% drop_na(response) %>%pull(key), collapse = "_")
    values$order <- paste(results %>% drop_na(response) %>%pull(order), collapse = "_")
    values$item_number <- paste(results %>% drop_na(response) %>%pull(item_number), collapse = "_")
  }
}