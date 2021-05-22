app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$setInputs(name = "name")
app$setInputs(notes = "notes")
app$setInputs(date = 1620133200000)
app$setInputs(start = "click")

# what responses do you want
responses <- rep(c("1", "2"), 5)

# key presses
for(i in 1:length(responses)){
  app$setInputs(keys = responses[i])
  app$setInputs(enter_key = "click")
}
app$snapshot()
vals <- app$getAllValues()
saveRDS(vals, file = here("tests", "test_dat.RDS"))

