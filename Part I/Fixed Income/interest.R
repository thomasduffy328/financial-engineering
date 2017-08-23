InterestCalc <- function(A, n, r, compound = T) {
  # an amount A is invested for n periods
  # at an interest of r
  # returns value after n periods depending on simple or compound interest
  if(compound == T) {
    return(A * (1 + r)^n)
  } else {
    return(A * (1 + n * r))
  }
}

pvFactor <- function(n, r) {
  # calculate the present value factor
  # for n periods and a fixed interest rate, r
  return((1 + r)^-n)
}

LumpSum <- function(C, n, r) {
  # calculate the present value, pv, of an lump sum C
  # paid over n years with interest rate, r
  return(C * pvFactor(n, r))
}

netPresentValue <- function(flows, n, r) {
  # calculate the net present value, npv, of a stream
  # of cash flows, flows, for n periods
  # assumes fixed interest rate, r
  if(mode(flows) == "numeric") {
    # here we're accounting for fixed cash flow
    flows <- rep(flows, n)
  }
  if(n != length(flows)) {
    stop("There must be as many cash flows as periods")
  } else {
    values <- vector("numeric", n)
    for(i in 0:(n-1)) {
      values[i + 1] <- flows[i + 1] * pvFactor(i, r)
    }
    npv <- sum(values)
  }
  return(npv)
}

PresentValue <- function(C0, rb, rl, N, borrow = T) {
  # the present valu, pv, of the contract that pays c.sub cash
  # flows for N periods
  # assuming fixed interest rates, rl (lending) & rb (borrowing)
  # that are not necessarily equal
  pv <- C0
  c.sub <- vector("numeric", N + 1)
  c.sub[1] = C0

  # price of borrowed contract 
  # price = p - c0 - sum(k=1 to N) ck/(1+rb)^k
  
  # price of sold contract
  # price = -p + c0 + sum(k=1 to N) ck/(1+rl)^k
  
  if(rl == rb) {
    if(borrow == T) {
      for(i in 2:N) {
        pv = pv + c.sub[i]/((1 + rl)^i) 
      }
    } else {
      
    }
    
  } else {
    if(borrow == T) {
      
    } else {
      
    }
  }
  return(pv)
  
  # NOTE: bounds on the price
  # PV(c; rb) <= p <= PV(c;rl)
  # again, rl <= rb
  # this provides us with a price interval
  # then supply & demand determine the price within that interval
}

# FIXED INCOME SECURITIES - 13:51
