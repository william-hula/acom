# This script calls the testing script. 
# must clear the current folder before using. 
library(shinytest)
# creates a folder for today if it doesnt exist.
# if it does exist, IT WILL DELETE THE FILES IN IT, WHICH IS NECESSARY TO RUN ANOTHER TEST.
if(dir.exists(here("tests", "test_output", Sys.Date()))){
  files = list.files(here("tests", "test_output", Sys.Date()), full.names = T)
  file.remove(files)
} else {
dir.create(here("tests", "test_output", Sys.Date()))
}

shinytest::testApp(here::here())

