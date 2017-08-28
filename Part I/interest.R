# Interest -----------------------------------------

InterestReturn <- function(A, n, r, compound = T) {
  # an amount A is invested for n periods
  # at an interest of r
  # returns value after n periods depending on simple or compound interest
  if(compound == T) {
    return(A * (1 + r)^n)
  } else {
    return(A * (1 + n * r))
  }
}

# Present Value ------------------------------

pvFactor <- function(n, r) {
  # calculate the present value factor
  # for n periods and a fixed interest rate, r
  return((1 + r)^-n)
}

PresentValue <- function(flows, rb, rl, N) {
  # determines the present value of a portfolio of cash flows (flows)
  # over N periods with a borrowing interest rate, rb, and a 
  # a lending interest rate, rl
  if(length(flows) == 1) {
    flows <- rep(flows, N)
  } else if(length(flows) != N) {
    stop("There must be the same number of cash flows as periods")
  }
  if(rl == rb) {
    pv <- flows[1]
    for(i in 2:N) {
      pv <- pv + flows[i] * pvFactor(i-1, rb)
    }
  output <- list("Value" = pv)
  return(output)
  } else {
    pv.borrow <- flows[1]
    pv.lend   <- flows[1]
    for(i in 2:N-1) {
      pv.borrow <- pv.borrow + flows[i] * pvFactor(i-1,rb)
      pv.lend   <- pv.lend + flows[i] * pvFactor(i-1, rl)
    }
  output <- list("Borrow Value" = pv.borrow, "Lend Value" = pv.lend)
  return(output)
  }
}

# Forward Pricing ----------------------------------

DiscountRate <- function(t, r, compounded = 1) {
  # returns the discount rate over t years with
  # annual interest rate, r
  # where compounded represents times compounded per annum
  discount <- 1/((1 + r/compounded)^(t * compounded))
  return(discount)
}

# should you add something in value return about the position you're
# taking, b/c the direction +/- is dependent on that
# if long S - fprice; if short fprice - S (refer to Forward Contracts, ~11:00)
ForwardPrice <- function(S, r, years, compounded = 1) {
  # calculates forward price of an asset, S
  # given an annual interest rate, r, that
  # is compounded n periods 
  # compounded: amount of times compounded per year
  fprice <- S/DiscountRate(years, r, compounded)
  value  <- S - fprice # this part needs updatin\g
  output <- list('Forward Price' = fprice, 'Value at T' = value)
  return(output)
}


