# library(magrittr)
# library(stringr)
# library(readr)
# 
# number_of_lines_of_code <- function(file_path){
#   file <- readr::read_file(file_path)
#   file_lines <- file %>% stringr::str_split("\n") 
#   first_character_of_lines <- file_lines %>%
#     lapply(function(line)stringr::str_replace_all(line," ",""))  %>% 
#     lapply(function(line)stringr::str_sub(line,1,1)) %>%
#     unlist
#   sum(first_character_of_lines != "#" & first_character_of_lines != "\r")
# }

#number_of_lines_of_code(here("R", "app.R"))
