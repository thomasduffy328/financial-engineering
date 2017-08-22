# ---------------
# LINEAR PRICING
# ---------------

# assume cash flows are annual and annual interest rate = r

discount_rate <- function(r, t) {
  # calculates discount rate up to time T
  # r = interest rate(s)
  # t = time periods
  return(1/((1 + r)^t)) 
  
}

present_value <- function(c, r, t, fixed) {
  # calculates ct = price of a contract that pays cash flows c0, c1, ... , cn
  # c = cash flow per period
  # r = annual interest rate(s)
  # t = time periods
  # ct = price of contract at t = 0
  
  # error handling if there are more time periods than int. rates
  if(length(r) > 1 & length(t) < length(r)) {
    stop('You not defined enough rates for the time period entered')
  }
  
  if(length(r) == 1) {
    # for fixed rate
    ct <- c * discount_rate(r, t)
  } else {
    # for floating rate
    ct <- c 
  }
  
  return(ct)
}
