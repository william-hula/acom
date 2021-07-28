# This script calls the testing script. 
# must clear the current folder before using. 
library(shinytest)
library(here)
# creates a folder for today if it doesnt exist.
dirs = c("irt_30", "pnt_175", "walker_a", "walker_b")

for(i in dirs){
    if(dir.exists(here("tests", "test_output", i, Sys.Date()))){
      files = list.files(here("tests", "test_output", i, Sys.Date()), full.names = T)
      file.remove(files)
    } else {
    dir.create(here("tests", "test_output", i,  Sys.Date()))
    }
}

# best to pick one or the other
# pnt-175 need to get data for. also takes a while...
#test = c("pnt-175", "pnt-30")
test = c("pnt-30", "pnt-walker-a", "pnt-walker-b", "pnt-175")

shinytest::testApp(here::here(),testnames = test, compareImages = FALSE, interactive = FALSE)

