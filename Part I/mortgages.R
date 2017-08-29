# Amortization --------------------------

BalanceLeft <- function(M, c, n, k) {
  # tells us the value of outstanding mortgage principal on a mortgage M
  # after k periods of the n total periods
  # with fixed coupon rate, c
  
  Mk <- M * ((1 + c)^n - (1 + c)^k)/((1 + c)^n - 1)
  return(Mk)
}

FindPayment <- function(M, c, n) {
  # solve for fixed payment of a mortgage, M, amortizing over n periods
  # with fixed coupon rate, c
  
  payment <- ((c * (1 + c)^n)*M)/((1 + c)^n - 1)
  return(payment)
}

PaymentBreakdown <- function(M, c, n, k) {
  # finds the breakdown of a particular payment
  # into the components of interest payment and 
  # principal payment for a mortgage, M, and a 
  # fixed coupon rate, c, where n is the total number of periods
  # and we want the breakdown for the kth period
    
  principal <- FindPayment(M, c, n) - c * BalanceLeft(M, c, n, k-1) 
  interest <- c * BalanceLeft(M, c, n, k-1)
  output <- list(Principal = principal, Interest = interest)
  return(output)
}

MortgagePV <- function(M, c, r, n) {
  # assuming a deterministic situation with NO defaults or prepayments
  # calculate the fair mortgage value, fv, of a mortgage, M, in an 
  # environment with interest rate, r, and coupon rate, c
  # after n periods of payment
  
  fv <- (c * (1 + c)^n * M)/((1 + c)^n - 1) * ((1 + r)^n - 1)/(r * (1 + r)^n)
  return(fv) 
}

CashFlows <- function(M, c, r, n) {
  # returns the cash flows: beginning monthly balance, monthly payment,
  # interest & init balance paid per month for a mortgage, M, 
  # over n months, with fixed coupon rate, c, and fixed interest
  # rate, r
  
  payment <- FindPayment(M, c/12, n)
  # initialize a data frame to fill
  df <- data.frame(BeginningBalLeft = rep(NA,n), Payment = NA,
                   InterestPaid = NA, PrincipalPaid = NA)
  df[1,] <- c(M, payment, PaymentBreakdown(M, c/12, n, 1)[[2]], 
              PaymentBreakdown(M, c/12, n, 1)[[1]])
  i <- 2
  while(i < n) {
    df$BeginningBalLeft[i] <- BalanceLeft(M, c/12, n, i)
    df$PrincipalPaid[i] <- PaymentBreakdown(M, c/12, n, i)[[1]]
    df$InterestPaid[i] <- PaymentBreakdown(M, c/12, n, i)[[2]]
    i <- i + 1
  }
  df$Payment <- payment
  return(df)
}

# test case from spreadsheet
# CashFlows(M = 100000, c = .08125, r = .08125, n = 360)

# Prepayment Risk --------------------------

SMM <- function(CPR) {
  # returns the conditional prepayment rate (annual) expressed as % of current
  # outstanding principal in mortgage pool from input of single-month mortality rate
  
  return(1 - (1 - CPR)^(1/12))
}

CPR <- function(PSA, t) {
  # returns the conditional prepayment rate (annual) given the level of PSA and 
  # the number of periods, t
  
  if(t <= 30) {
    return((.06*PSA) * (t/30))
  } else {
    return(.06 * PSA)
  }
}

# Mortgage Backed Securities --------------------

MBSPassThru <- function(bal, c, r, payment, seasoning, term, PSA) {
  # seasoning = how old the mortgage pool currently is
  # bal = init balancy
  # c = monthly interest paid in by mortgage holders to the institution
  # r = monthly interest paid out by mortgage holders to investors
  # c > r b/c the institution assuming the risk has to be compensated (and for fees)
  # payment = init monthly payment
  # term = length of underlying mortgage(s)
  # PSA = multiplier for PSA 
  
  CPRvec <- vector("numeric")
  for(i in 1:term) {
    CPRvec[i] <- CPR(PSA, t = seasoning + i)
  }
  initholdpay <- bal * (c/12)
  initinvpay  <- bal * (r/12)
  initprepay  <- (bal - (payment - initholdpay)) * SMM(CPRvec[1])
  df <- data.frame(CPR = CPRvec, SMM = SMM(CPRvec), BeginBal = bal, Payment = payment, 
                   HolderGets = initholdpay, InvestorGets = initinvpay, 
                   PrincipalPaid = payment - initholdpay, 
                   Prepayment = initprepay, 
                   TotalPrincipalPaid = initprepay + (payment - initholdpay), 
                   EndBal = bal - (initprepay + (payment- initholdpay)))
  
    for(i in 2:term) {
    df$BeginBal[i]           <- df$EndBal[i - 1]
    df$Payment[i]            <- (df$EndBal[i - 1] * (c/12))/(1 - (1 + (c/12))^(-(term - seasoning - i + 1))) 
    df$HolderGets[i]         <- (df$BeginBal[i] * (c/12))
    df$InvestorGets[i]       <- (df$BeginBal[i] * (r/12))
    df$PrincipalPaid[i]      <- df$Payment[i] - df$HolderGets[i]
    df$Prepayment[i]         <- (df$BeginBal[i] - (df$Payment[i] - df$PrincipalPaid[i])) * df$SMM[i]
    df$TotalPrincipalPaid[i] <- df$PrincipalPaid[i] + df$Prepayment[i]
    df$EndBal[i]             <- df$BeginBal[i] - df$TotalPrincipalPaid[i]
    i <- i + 1
    }
  # needs fixing
  # round all digits reported to 4 figures
  apply(X = df, MARGIN = 2, FUN = round, digits = 4)
  return(df)
}

# test case from spreadsheet
# MBSPassThru(400, .08125, .075, 2.984, 3, 360, 1)

PrincipalPV <- function(M, c, r, n) {
  # determine the present value of all principal payments related to a 
  # portfolio of mortgages with value at t = 0 of M
  # where c & r are the fixed coupon & interest rates 
  # and n is the term 
  
  V0 <- ((c * M)/((1 + c)^n - 1)) * (((1 + r)^n - (1 + c)^n)/((r-c) * (1 + r)^n))
  return(V0)
}

InterestPV <- function(M, c, r, n) {
  # returns the present value of all interest payments related to a 
  # portfolio of mortgages 
  # noting that the PV of a mortgage is the sum of the PV of interest payments
  # and principal payments, use this sum to determine value of interest payments
  # where M is value of mortgages at t=0, c & r the fixed coupon & interest rates
  # and n is the term
  
  return(MortgagePV(M, c, r, n) - PrincipalPV(M, c, r, n))
}

# you can also use the PaymentBreakdown() function to do something similar
# perhaps that would be faster in computing the PV of a vector?


  

