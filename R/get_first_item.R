#' obtains the first item for the test
#' 
#' because the tests are different and have different first items,
#'
#' @param all_items at this stage, a dataframe of all possible items
#' @param previous are there previous items to exclude
#' @param exclude_previous whether or not to exclude those previous items
#' 
#' @return a slide number for the first item
#' 
#' @export
get_first_item <- function(all_items, previous, exclude_previous = F){ 
  
  #choices = c(130, 25, 39, 154)
  
  if(!exclude_previous){
    
    # if we want to sample the four anyway
    # first_item = sample(choices, 1)
    # return(first_item)
    return(130)

  } else {

    completed = previous

    all_items$response[match(completed$item_number, all_items$item_number)] <- completed$response
    # dataframe of inputs
    pars = data.frame(a = all_items$discrimination,
                      b = all_items$itemDifficulty,
                      c = rep(1), #1PL has no guessing parameter ,
                      d = rep(0), #1PL has no innatention parameter,
                      cbGroup = rep(1))
    
    # breaks it down into what gets fed into the 1PL IRT
    prov = catR::breakBank(pars) 
    bank = prov$itemPar
    rownames(bank) <- all_items$target
    x = all_items$response
    
    
    # ability estimate using bayes modal:
    ability = catR::thetaEst(bank, x, method = "EAP", range = c(-5, 5))
    first_item = catR::nextItem(itemBank = bank, theta = ability, out = completed$item_number)
    
    first_slide_num = all_items[all_items$target==first_item$name,]$slide_num
    
    return(first_slide_num)
  }
  
  
}


