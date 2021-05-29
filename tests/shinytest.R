# This script calls the testing script. 
# must clear the current folder before using. 
library(shinytest)
library(here)
# creates a folder for today if it doesnt exist.
# if it does exist, IT WILL DELETE THE FILES IN IT, WHICH IS NECESSARY TO RUN ANOTHER TEST.
if(dir.exists(here("tests", "test_output", Sys.Date()))){
  files = list.files(here("tests", "test_output", Sys.Date()), full.names = T)
  file.remove(files)
} else {
dir.create(here("tests", "test_output", Sys.Date()))
}

# best to pick one or the other
# pnt-175 need to get data for. also takes a while...
#test = c("pnt-175", "pnt-30")
test = "pnt-30"

shinytest::testApp(here::here(),testnames = test, compareImages = FALSE)

