# Swaps -------------

DiscountRate <- function(t, r, compounded = 1) {
  # returns the discount rate over t years with
  # annual interest rate, r
  # where compounded represents times compounded per annum
  discount <- 1/((1 + r/compounded)^(t * compounded))
  return(discount)
}

ValueSwap <- function(N, t, rates) {
  # determine the value of a swap for two counterparties (taking long & short positions)
  # with an N notional principle over a vector, rates, of interest rates over a 
  # time period, t
  if(t != length(rates)) {
    stop("There must be an interest for every t")
  }
  
  rate.sum <- 0
  for(i in 1:t) {
    rate.sum <- rate.sum + DiscountRate(i,rates[i])
  }
  X <- (1 - DiscountRate(t,rates[t]))/rate.sum
  
  # then calculate value dependent on position
  long.value <- (N * (1 - DiscountRate(t,rates[t]))) - (N * X * rate.sum)
  short.value <- (N * X * rate.sum) - (N * (1 - DiscountRate(t,rates[t])))

  # now construct output list
  output <- list('Short Value' = short.value, 'Long Value' = long.value)
  return(output)
}

SwapIntRateMin <- function(N, t) {
  # uses the function, ValueSwap(), to perform minimization to determine appropriate 
  # interest rates such that that Value of the swap for two counterparties is 
  # at or near zero
  # for a swap with notional principle, N, for a number of periods, t
  
    
}

# Futures -----------

FutureSim <- function() {
  # initialize the data frame
  df <- data.frame(Date = Sys.Date(), price = 0, profit = 0, 'margin account' = 0,
                   'margin call' = 0)
  
}
