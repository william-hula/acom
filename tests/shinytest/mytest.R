app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$setInputs(welcome_next = "click")
app$setInputs(administer_test = "click")
app$setInputs(numitems = "175_cat")
app$setInputs(next_test = "click")
app$uploadFile(file_incomplete = "resume_pntcat175_key.csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(resume = "click")
app$snapshot()
