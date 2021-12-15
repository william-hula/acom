#' Generates faq for accordions
#'
#' @export
accordion_test <- function(){
  
  bs_accordion(
    id = "accordion_test",
    items = tagList(
      bs_accordion_item(
        title = "PNT-CAT30: 30-item Computer Adaptive Version (Fergadiotis et al., 2019):",
        div(
          tags$p("This test adaptively administers 30 items, beginning with an item targeting average naming ability. Items are selected to provide the maximum reduction in the standard error of measurement based on the cumulative responses collected. Score estimates from the PNT-CAT30 correlated 0.95 with scores from the full PNT and are suitable for estimating the overall severity of anomia in persons with aphasia. Change in naming ability can be assessed using a subsequent PNT-CAT30 or Variable Length PNT-CAT."),
        tags$b("Average administration time: 8-9 Minutes.")
        ),
        active = TRUE
      ),
      bs_accordion_item(
        title = "PNT-30A, PNT-30B: 30-item static short forms A & B (Walker & Schwartz, 2012):",
        div(
          tags$p("These are two static 30-item PNT short forms with non-overlapping items. The two short forms correlated highly with each other (0.93), and with the full PNT (0.93 and 0.98). This application provides T-score estimates for these short forms on the same scale as the other test versions. Change in naming ability can be assessed using the alternate form."),
        tags$b("Average administration time: similar to PNT-CAT30.")
        )
        
      ),
      bs_accordion_item(
        title = "PNT-CAT175: Full 175-item version in adaptive order (Fergadiotis et al., 2019):",
        div(
          tags$p("This test provides administration of (up to) all 175 items in adaptive order, beginning with an item targeting average naming ability, and proceeding as described above for the PNT-CAT30. This test should be used when a more, or maximally precise score estimate is required relative to the PNT-CAT30. Users can stop this test at any point during administration. Provided that at least 30 items were administered, this test should have psychometric properties similar to or better than those reported for the PNT-CAT30. "),
        tags$b("Average administration time: 20-40 minutes.")
        )
      ),
      bs_accordion_item(
        title = "PNT-175: Full 175-item version* in standard administration order (Roach et al., 1996):",
        div(
          tags$p("This option provides administration of all 175 items in standard PNT administration order. Although this test can also be stopped at any point during administration and will provide a theoretically valid score estimate and standard error, this is not recommended because we have not collected evidence regarding the validity of short form score estimates obtained in this way."),
        tags$b("Average administration time: Similar to PNT-CAT175.")
        )
      )
    )
  )
  
}


#' Generates faq for accordions
#'
#' @export
accordion_retest <- function(){
  
  bs_accordion(
    id = "accordion_retest",
    items = tagList(
      bs_accordion_item(
        title = "PNT-CAT30: 30-item Computer Adaptive Version (Fergadiotis et al., 2019):",
        div(
          tags$p("This test adaptively administers 30 items, beginning with an item targeting average naming ability, excluding those delivered in a previous PNT-CAT30 (recommended). Items are selected to provide the maximum reduction in the standard error of measurement based on the cumulative responses collected. Score estimates from the PNT-CAT30 correlated 0.95 with scores from the full PNT and are suitable for estimating the overall severity of anomia in persons with aphasia. Average administration time: 8-9 Minutes."),
          tags$b(" Required upload: A previous PNT-CAT30.")
        ),
        active = TRUE
      ),
      bs_accordion_item(
        title = "PNT-CATVL: Variable-length Computer Adaptive Alternate Form (Hula et al., 2020):",
        div(
          tags$p("This test adaptively administers items until the standard error of the score estimate is as precise as it was for a previously administered PNT-CAT30. It also excludes the items given in the previous PNT-CAT30 (required). Score estimates from the PNT-CATVL correlated 0.89 with independent estimates from the PNT-CAT30. Although the PNT-CATVL administers up to 100 items, the median number of items administered by Hula et al. (2020) was 43.5 and the minimum number was 21. Average administration time: 9-10 minutes."),
          tags$b("Required upload: A previous PNT-CAT30.")
        )
      ),
      bs_accordion_item(
        title = "PNT-30A, PNT-30B: 30-item static short forms A & B (Walker & Schwartz, 2012):",
        div(
          tags$p("These are two static 30-item PNT short forms with non-overlapping items. For a second assessment, the alternative form should be chosen. The two short forms correlated highly with each other (0.93), and with the full PNT (0.93 and 0.98). This application provides T-score estimates for these short forms on the same scale as the other test versions. Average administration time: similar to PNT-CAT30."),
          tags$b("Required upload: A previous PNT-30A or PNT-30B.")
        )
      ),
      bs_accordion_item(
        title = "PNT-175: Full 175-item version* in standard administration order (Roach et al., 1996):",
        div(
          tags$p("This option provides administration of all 175 items in standard PNT administration order. Although this test can also be stopped at any point during administration and will provide a theoretically valid score estimate and standard error, this is not recommended because we have not collected evidence regarding the validity of short form score estimates obtained in this way."),
          tags$b(" Required upload: A Previous PNT-175.")
        )
      )
    )
  )
  
}

#' Generates faq for accordions
#'
#' @export
accordion_rescore <- function(){
  
  # bs_accordion(
  #   id = "accordion_page1",
  #   items = tagList(
  #     bs_accordion_item(
  #       title = "PNT-CAT30: 30-item Computer Adaptive Version (Fergadiotis et al., 2019):",
  #       tags$p("This test adaptively administers 30 items, beginning with an item targeting average naming ability. Items are selected to provide the maximum reduction in the standard error of measurement based on the cumulative responses collected. Score estimates from the PNT-CAT30 correlated 0.95 with scores from the full PNT and are suitable for estimating the overall severity of anomia in persons with aphasia. Change in naming ability can be assessed using a subsequent PNT-CAT30 or Variable Length PNT-CAT. Average administration time: 8-9 Minutes. "),
  #       active = TRUE
  #     ),
  #     bs_accordion_item(
  #       title = "PNT-30A, PNT-30B: 30-item static short forms A & B (Walker & Schwartz, 2012):",
  #       tags$p("These are two static 30-item PNT short forms with non-overlapping items. The two short forms correlated highly with each other (0.93), and with the full PNT (0.93 and 0.98). This application provides T-score estimates for these short forms on the same scale as the other test versions. Change in naming ability can be assessed using the alternate form. Average administration time: similar to PNT-CAT30.")
  #     ),
  #     bs_accordion_item(
  #       title = "PNT-CAT175: Full 175-item version in adaptive order (Fergadiotis et al., 2019):",
  #       tags$p("This test provides administration of (up to) all 175 items in adaptive order, beginning with an item targeting average naming ability, and proceeding as described above for the PNT-CAT30. This test should be used when a more, or maximally precise score estimate is required relative to the PNT-CAT30. Users can stop this test at any point during administration. Provided that at least 30 items were administered, this test should have psychometric properties similar to or better than those reported for the PNT-CAT30. Average administration time: 20-40 minutes."),
  #     ),
  #     bs_accordion_item(
  #       title = "PNT-175: Full 175-item version* in standard administration order (Roach et al., 1996):",
  #       tags$p("This option provides administration of all 175 items in standard PNT administration order. Although this test can also be stopped at any point during administration and will provide a theoretically valid score estimate and standard error, this is not recommended because we have not collected evidence regarding the validity of short form score estimates obtained in this way."),
  #     ),
  #   )
  # )
  # 
}

#' Generates faq for accordions
#'
#' @export
accordion_faq <- function(){
  
  bs_accordion(
    id = "accordion_faq",
    items = tagList(
      bs_accordion_item(
        title = "How long should I wait for a response?",
        tags$p("While the standard and short-form PNT administration rules (Roach et al., 1996; Walker and Schwartz 2012) allow examinees up to 30 seconds to respond to each item, studies for the PNT-CAT short forms (Fergadiotis et al., 2019; Hula et al., 2020) allowed only 10 seconds."),
        active = TRUE
      ),
      bs_accordion_item(
        title = "When can I download the data?",
        tags$p("You can download results after the first (non-practice) item has been scored in case you need to end the test early. ")
      ),
      bs_accordion_item(
        title = "I entered the wrong response. Can I go back and fix it?",
        tags$p("While you cannot go back to a previous item, you can download the results at the end of the test, revise the scoring for any items, and re-upload the file under the offline testing function to re-estimate the final score. "),
      ),
      bs_accordion_item(
        title = "What if I need to take a break? / Can I resume an incomplete test?",
        tags$p("For the Standard PNT, examinees are often allowed a break between items #88 (tractor”) and #89 (queen) if necessary. If running the app locally, the app can be left running. However, if you are using the web/browser-based version of the app, it will time-out after 10 minutes of non-use. Therefore, for the free online version, it is best to end the test and download data. When you are ready to continue, you can upload the downloaded file (unmodified!) on this page to continue the test. Make sure to make the same test administration selections. See https://aphasia-apps.github.io/pnt/articles/articles/pnt.html for instructions on running the app locally. In general, we recommend this option for administrations of the 175-item PNT and for reserach studies."),
      ),
      bs_accordion_item(
        title = "Should I audio record the test?",
        tags$p("It is generally recommended that examiners audio record testing sessions to preserve responses for later review, as was recommended in the standard PNT instructions."),
      ),
      bs_accordion_item(
        title = "Should I give feedback after a response?",
        tags$p("The standard PNT instructions suggest giving the examinee feedback after a response. However, giving feedback after each response may lead to undesired item learning if repeated administrations of the same items are anticipated (e.g. re-administering the standard PNT175). Studies for the PNT-CAT did not provide feedback. However, for situations where items will not be re-adminsitered (e.g. PNT-CAT30 and PNT-CAT VL), whether to give feedback is up to the discretion of the clinician.")
      ),
      bs_accordion_item(
        title = "How can I use the app on an iPad or other tablet device?",
        tags$p("On a tablet, tap on the center image to advance to the next item (equivalent to pressing enter). Just like on a computer, you must record a resposne for any naming trials. To record a response, tap the far right or far left edge of the screen (either side is fine). Tap once for incorrect (equivalent to pressing 1) and again for correct (equivalent to pressing 2). You can continue to tap on the edge of the screen to toggle between these responses. The same response feedback will be available in the top right of the screen.")
      ),
      bs_accordion_item(
        title = "Where can I find the original PNT Instructions?",
        div(
          tags$p("These instructions have been adapted from the original PNT instructions. They can be found at mrri.org:"), tags$a("https://mrri.org/wp-content/uploads/2016/01/PNT-Admin-Instructions.pdf")
        )
      )
    )
  )
  
}