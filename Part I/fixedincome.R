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

PresentValue <- function(c, n, r, peryear = 1) {
  
  # if not given vector of cash flows, but a fixed cash flow
  if(length(c) != n) {
    c <- rep(c, n)
  }
  # if not given a vector of interest rates, but a fixed int. rate
  if(length(r) != n) {
    r <- rep(r, n)
  }
  pv <- sum(c * (1 + r)^(0:(n-1)))
  return(pv)
}

NPV <- function(A, n, r, compound) {
  # determines the net present value of A cash flows at t=0
  # that accrue over n periods (years) and 
  # with a fixed interest rate, r
  # and compound is the number of times compounded per year
  # assumes lending & borrowing rates are equal
  
  price <- A * DiscountRate(n, r, compound)
  return(price)
}

FuturePrice <- function() {
  
}

# Fixed Income Instruments ----------------