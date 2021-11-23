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
  
  items$near_zero = abs(50-items$itemDifficulty)
  
  if(!exclude_previous){
    
    next_slide_num = items[items$near_zero == min(items$near_zero),]$slide_num
    
  } else {

    # pick the item closest to zero from the remaining items (0 == 50 in T scores)
    items$response[match(previous$item_number, items$item_number)] <- previous$response
    remaining_items = subset(items, is.na(response))
    next_slide_num = remaining_items[remaining_items$near_zero == min(remaining_items$near_zero),]$slide_num

  }
  
  return(next_slide_num)
  
  
}


