#' get first item
#'
#' @param all_items out
#' @param previous prev
#' @param exclude_previous num prev
#' @export
get_first_item <- function(all_items, previous, exclude_previous = F){ 
  
  #choices = c(130, 25, 39, 154)
  
  if(!exclude_previous){
    
    # if we want to sample the four anyway
    # first_item = sample(choices, 1)
    # return(first_item)
    return(130)

  } else {
    

    completed = previous %>%
      group_by(date) %>%
      mutate(num = cur_group_id()) %>%
      filter(num == max(num)) %>%
      select(-num) %>%
      drop_na(response)
    
    all_items$response[match(completed$item_number, all_items$item_number)] <- completed$response
    # dataframe of inputs
    pars = data.frame(a = all_items$discrimination,
                      b = all_items$itemDifficulty,
                      c = rep(1), #1PL has no guessing parameter ,
                      d = rep(0), #1PL has no innatention parameter,
                      cbGroup = rep(1))
    
    # breaks it down into what gets fed into the 1PL IRT
    prov = breakBank(pars) 
    bank = prov$itemPar
    rownames(bank) <- all_items$target
    x = all_items$response
    
    
    # ability estimate using bayes modal:
    ability = thetaEst(bank, x, method = "EAP", range = c(-5, 5))
    first_item = nextItem(itemBank = bank, theta = ability, out = completed$item_number)
    
    first_slide_num = all_items[all_items$target==first_item$name,]$slide_num

    return(first_slide_num)
  }
  
  
}



# } else {

# first_item <- 
#   tibble(choices = choices) %>%
#   filter(choices %!in% previous_dat$slide_num) %>%
#   slice_sample(n = 1) %>%
#   pull(choices)
# 
# print(first_item)
# return(first_item)