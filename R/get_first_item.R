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
    
    #pumpkin
    return(130)

  } else {

    # pick the item closest to zero from the remaining items (0 == 51.9 in T scores)
    all_items$response[match(previous$item_number, all_items$item_number)] <- previous$response
    
    remaining_items = subset(all_items, is.na(response))
    remaining_items$near_zero = abs(51.9-remaining_items$itemDifficulty)
    next_slide_num = remaining_items[remaining_items$near_zero == min(remaining_items$near_zero),]$slide_num
    

    print(head(remaining_items[order(remaining_items$near_zero), , drop = FALSE], 20))
   
    return(next_slide_num)

  }
  
  
}


