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

#' Capital Expenditures
#'
#' This function calculates the CapEx given projected revenue and 
#' @param revenue vector of projected revenue for each year post-transaction
#' @param rate Cap Ex investment rate (% of sales) ex: 2% would be .02
#' @keywords 
#' @export
#' @examples
#' capEx(c(1610.51,	1771.561,	1948.7171,	2143.58881,	2357.947691), .02)

capEx <- function(revenue, rate){
  print("CapEx: ----------------------------------------")
  year <- 0
  for(value in revenue){
    year <- year +1
    value <- value * rate
    cat("Year", year, ": ", value, " ")
  }
}






