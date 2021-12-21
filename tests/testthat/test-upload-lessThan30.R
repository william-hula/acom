test_that("score re-uploaded test PNT-CAT30", {

  #########################################################
  # Get app to results page
  #########################################################

  app <- ShinyDriver$new(here::here(), seed = 1)
  #app$setInputs(welcome_next = "click")
  app$setInputs(score_test = "click")
  app$uploadFile(file2 = here::here("tests", "testthat", "files", "test_upload_lessThan30.csv"))
  app$setInputs(score_uploaded_data = "click")
  Sys.sleep(4)
  val = app$getAllValues()

  #########################################################
  # TESTS
  #########################################################
  
  # are we on the results page?
  testthat::expect_equal(val$export$current_page, "Results")

  testthat::expect_true(grepl('<em style="color:darkred;">', val$output$results_summary$html))

})
