# ---------------
# LINEAR PRICING
# ---------------

# assume cash flows are annual and annual interest rate = r

discount_rate <- function(r,t) {
  # calculates discount rate over to T
  # r = interest rate
  
  return(1/((1 + r)^t))
}

present_value <- function(c, r, t) {
  # c = original cash flow
  # r = annual interest rate(s)
  # t = time periods
  # ct = price of cash flow
  
  # error handling if there are more time periods than int. rates
  if(length(r) > 1 & length(t) < length(r)) {
   stop('You not defined enough rates for the time period entered')
  }
  
  # for fixed rate
  if(length(r) == 1) {
    ct <- c * discount_rate(r, t)
  }
  
  return(ct)
}



