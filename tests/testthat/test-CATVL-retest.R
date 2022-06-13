test_that("PNT-CATVL-retest", {
  
  #########################################################
  # Get app to results page
  #########################################################
  
  app <- ShinyDriver$new(here::here(), seed = 1)
  responses <- c(rep(c(1,2), 15), 1)
  
  #app$setInputs(welcome_next = "click")
  app$setInputs(administer_test = "click")
  app$setInputs(retest = "click")

  app$uploadFile(file1 = here::here("tests", "testthat", "files", "test_upload_cat30.csv"))
  app$setInputs(numitems = "SEM")
  
  app$setInputs(next_test = "click")
  
  app$setInputs(start_practice = "click")

  app$executeScript("Mousetrap.trigger('enter');")
  app$executeScript("Mousetrap.trigger('enter');")
  
  for(i in 1:9){
    app$executeScript("Mousetrap.trigger('1');")
    app$executeScript("Mousetrap.trigger('enter');")
  }
  
  Sys.sleep(0.5)
  app$setInputs(start = "click")

  for(i in 1:length(responses)){
    if(responses[i]==1){
      app$executeScript("Mousetrap.trigger('1');")
    } else {
      app$executeScript("Mousetrap.trigger('2');")
    }
    app$executeScript("Mousetrap.trigger('enter');")
  }

  Sys.sleep(3)
  val = app$getAllValues()
  
  #########################################################
  # TESTS
  #########################################################
  
  # are we on the results page?
  testthat::expect_equal(val$export$current_page, "Results")
  # are responses tracked accurately? 
  testthat::expect_equal(sum(val$export$results$key=='2', na.rm = T), sum(responses==2))
  testthat::expect_equal(sum(val$export$results$key=='1', na.rm = T), sum(responses==1))
  # are both test administration final numbers saved?
  testthat::expect_equal(sum(!is.na(val$export$irt_final)), 4)
  # did the test stop when the second SEM was less than the first?
  testthat::expect_lt(val$export$irt_final$sem, val$export$irt_final$last_sem)
  # make sure SEM difference isn't crazy
  testthat::expect_lt((val$export$irt_final$last_sem-val$export$irt_final$sem), 1)
  # can we download data and the results?
  testthat::expect_gt(length(app$snapshotDownload("download_results-results_download")), 100)
  testthat::expect_gt(length(app$snapshotDownload("download_report-report_download")), 100)
})
