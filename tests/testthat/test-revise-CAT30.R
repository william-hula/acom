test_that("score re-uploaded test PNT-CAT30", {

  #########################################################
  # Get app to results page
  #########################################################

  app <- ShinyDriver$new(here::here(), seed = 1)
  app$setInputs(welcome_next = "click")
  app$setInputs(score_test = "click")
  app$uploadFile(file2 = here::here("tests", "testthat", "files", "test_upload_cat30.csv"))
  app$setInputs(score_uploaded_data = "click")
  Sys.sleep(3)
  val = app$getAllValues()

  #########################################################
  # TESTS
  #########################################################
  uploaded_file = read.csv(here::here("tests", "testthat", "files", "test_upload_cat30.csv"))
  
  
  # are we on the results page?
  testthat::expect_equal(val$export$current_page, "Results")
  # are responses tracked accurately? 
  testthat::expect_equal(sum(val$export$results$key=='2', na.rm = T),
                         sum(uploaded_file$key==2, na.rm = T))
  testthat::expect_equal(sum(val$export$results$key=='1', na.rm = T),
                         sum(uploaded_file$key==1, na.rm = T))
  # Are the test administration final numbers saved?
  testthat::expect_equal(sum(!is.na(val$export$irt_final)), 2)
  # Make sure 30 items were administered:
  testthat::expect_equal(
    sum(val$export$results$key=='2', na.rm = T)+sum(val$export$results$key=='1', na.rm = T),
    30)
  # can we download data and the results?
  testthat::expect_gt(length(app$snapshotDownload("download_results-results_download")), 100)
  testthat::expect_gt(length(app$snapshotDownload("download_report-report_download")), 100)


})
