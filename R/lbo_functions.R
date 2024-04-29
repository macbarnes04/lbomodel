#' Goodwill
#'
#' This function calculates the goodwill given amount paid for equity and net book value of assets 
#' @param equity Amount paid for equity
#' @param assets Less: Net Book Value of Assets
#' @keywords 
#' @export
#' @examples
#' goodwill(1047, 642)

goodwill <- function(equity, assets){
  result <-  equity - assets
  statement <- "Your Goodwill is:"
  output <- paste(statement, result)
  print(output)
}




