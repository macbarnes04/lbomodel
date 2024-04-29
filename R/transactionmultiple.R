

#' Transaction Multiple Estimator
#'
#' This function allows you to estimate a transaction multiple for a given company.
#' @param sector Which engineering sector is your company a part of? View list of current
#' supported sectors at https://github.com/macbarnes04/lbomodel 
#' @param performance On a percentile scale (1-99) compared to other companies you have seen, what is you expectation of this company meeting future performance expectations (99% meaning it would meet expectations better than 99% of current peer companies, esentially equating to a 99% confidence in meeting expectations). input as 0-1 (ex. 0.99 means 99 percentile)
#' @param RoIC On a percentile scale (1-99) how well has this company returned invested capital relative to its peers? (99 meaning it has a RoIC better than 99% of peers you have seen). Input as 0-1 (ex. 0.99 means 99 percentile)
#' @param risk On a percentile scale (1-99) what is your perceived risk of this company relative to its peers? input as 0-1 (ex. 0.99 means 99 percentile). 
#' @export
#' @examples
#' transaction_multiple(aerospace)

transaction_multiple <- function(sector, performance, RoIC, risk){
  base_loc <- "lbomodel/inst/"
  file_loc <- paste(base_loc, sector, ".xlsx", sep = "")
  data <- read_excel(file_loc)
  multiples <- data[[6]]
  dev <- sd(multiples)
  avg <- mean(multiples)
  
  # Calculate the weighted average
  weighted_avg <- (performance + RoIC + risk) / 3
  
  # Percentile calculation
  percentile <- weighted_avg  
  inverse_normal <- qnorm(percentile, mean = avg, sd = dev)
  statement <- paste("Your estimated transaction multiple is:", inverse_normal)
  print(statement)
}



