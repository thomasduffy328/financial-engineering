# Fixed Income Securities -----------------

InterestCalc <- function(A, n, r, compound = T, peryear = 1) {
  # returns the value of an amount A after an accrual of 
  # interest from an annual interest rate, r, over n years
  # where compound = T returns compounding interest 
  # or compound == F returns simple interest
  # peryear signifies the times interest compounds per year
  
  if(compound == F) {
    return(A * (1 + (n * peryear) * (r/peryear)))
  } else {
    return(A * (1 + (r/peryear))^(n * peryear))
  }
}

DiscountRate <- function(n, r, peryear = 1) {
  # determines the discount rate over n periods (years)
  # with a fixed annual interest rate of r
  # where peryear is the number of times compounded per year
  
  return(1/((1 + (r/peryear))^(n * peryear)))
}

PVFactor <- function(n, r) {
  # calculate the present value factor
  # for n periods and a fixed interest rate, r
  
  return((1+r)^(-n))
}

PresentValue <- function(c, n, rl, rb) {
  # returns the present value of a contract of cash flows, c
  # over n periods with annual lending rate, rl, and 
  # borrowing rate, rb, (where rl <= rb)
  
  # if not given vector of cash flows, but a fixed cash flow
  if(length(c) != n) {
    c <- rep(c, n)
  }
  # if not given a vector of interest rates, but a fixed int. rate
  if(length(rl) != n) {
    rl <- rep(rl, n)
  }
  if(length(rb) != n) {
    rb <- rep(rb, n)
  }
  
  if(all(rl == rb)) {
    pv <- sum(c * PVFactor(0:(n-1), rb))
    return(pv)
  } else {
    pvl <- sum(c * PVFactor(0:(n-1), rl))
    pvb <- sum(c * PVFactor(0:(n-1), rb))
    output <- list("Upper Bound (rl)" = pvl, "Lower Bound (rb)" = pvb)
    return(output)
  }
}

Perpetuity <- function(A, r) {
  # determine the present value of an annuity
  # paying A with fixed interest rate, r
  
  return(A/r)
}

Annuity <- function(A, n, r) {
  # determine the present value of an annuity paying A
  # for n periods (beginning immediately) with a vector of 
  # interest rates, r
  
  if(length(r) == 1) {
    r <- rep(r, n)
  }
  value <- 0
  for(i in 0:(n-1)) {
    value <- value + (A * PVFactor(i, r[i + 1]))
  }
  return(value)
}

FuturePrice <- function(A, n, r, peryear = 1) {
  # returns the future price of an asset priced 
  # at A at t = 0 for a time, n periods, in the future
  # with a fixed interest rate, r
  
  price <- A/(DiscountRate(n, r, peryear))
  return(price)
}



# Fixed Income Instruments ----------------