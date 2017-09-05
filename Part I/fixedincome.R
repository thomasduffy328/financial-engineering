# Fixed Income Instruments -----------------

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

# check this
DiscountRate <- function(n, r, peryear = 1, continuous = F) {
  # determines the discount rate over n periods
  # with a fixed annual interest rate of r
  # where peryear is the number of times compounded per year
  # if continuous == T, calculates the continous discount rate
  
  if(continuous == F) {
    return(1/((1 + (r/peryear))^(n * peryear)))
  } else {
    return(exp(-r * n))
  }
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

# Linear Pricing ----------------

CashPrice <- function(c, r, t) {
  # compute linear price of a cash flow that pays c at t=t
  # and 0 everywhere else
  # with an annual interest rate,r 
  
  return(c * DiscountRate(n = t, r = r))
}

# check this with an example
# note that r is a RANDOM quantity, so it is only known at time = k-1 
# so perhaps you need to build in a randomizer for the r vector
# because the interest rates WILL NOT be fixed at t=0
BondPrice <- function(payment, r, n) {
  if(length(r) == 1) {
    r <- rep(r, n)
  }

  payment <- rep(payment, n)
  pk <- sum(payment * DiscountRate(n, r))
  print(pk)
  big.p  <- payment[1] * DiscountRate(n, r[1]) 
  return(big.p + pk)
}

# at 18:35 the part about Discount rates and forward rates is 
# an important thing to code
# likely should use the slides to make it work 

# Forward Contracts -------------

