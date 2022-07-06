# # read in item parameters for full ACOM
# acom_itpar_content <- read.csv(file = here::here("dev", "acom_Tscaled_ItemParameters_Content.csv"),na.strings = "NA")
# acom_itpar_content$itnum <- as.numeric(seq(1:59))
# 
# acom_itpar <- acom_itpar_content[1:59,c(4:7,3)]
# acom_itpar <- data.matrix(acom_itpar)
# 
# bank = acom_itpar[,1:4]
# 
# # define options for content balancing
# acom_cb_names <- c("talk","comp","gen","writ","nam")
# acom_cb_props <- c(0.355932203, 0.271186441, 0.13559322, 0.13559322, 0.101694915)
# acom_cbControl <- list(names = acom_cb_names, props = acom_cb_props)
# acom_cb_group <- acom_itpar_content$content_area
# catR::test.cbList(acom_cbControl, cbGroup = acom_cb_group)
# 
# rowCallback <- c(
#   "function(row, data){",
#   "  for(var i=0; i<data.length; i++){",
#   "    if(data[i] === null){",
#   "      $('td:eq('+i+')', row).html('NA')",
#   "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
#   "    }",
#   "  }",
#   "}"
# )
# 
# 
# d = read.csv(here::here("dev", "acom_Tscaled_ItemParameters_Content.csv"))
# d$itnum <- as.numeric(seq(1:59))
# d$order = NA
# d$response = NA
# d$response_num = NA
# d$clarify = NA
# d$theta = NA
# d$sem = NA

usethis::use_data(
  jsCode,
  enter,
  end_test_key,
  d,
  rowCallback,
  bank,
  acom_cb_group,
  acom_cbControl,
  acom_cb_names,
  internal = T, overwrite = T)
