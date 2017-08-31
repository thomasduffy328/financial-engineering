# MAKE SURE TO FIX ALL THE COUPON RATES 
# confusion of monthly v. annual in some places
# also check to see if vectorization anywhere is faster than looping  

# Amortization --------------------------

BalanceLeft <- function(M, c, n, k) {
  # tells us the value of outstanding mortgage principal on a mortgage M
  # after k periods of the n total months
  # with fixed coupon rate, c
  
  c  <- (c/12)
  Mk <- M * ((1 + c)^n - (1 + c)^k)/((1 + c)^n - 1)
  return(Mk)
}

FindPayment <- function(M, c, n, months = T) {
  # solve for fixed payment of a mortgage, M, amortizing over n months
  # with fixed annual coupon rate, c
  
  c       <- (c/12)
  payment <- ((c * (1 + c)^n)*M)/((1 + c)^n - 1)
  return(payment)
}

PaymentBreakdown <- function(M, c, n, k) {
  # finds the breakdown of a particular payment
  # into the components of interest payment and 
  # principal payment for a mortgage, M, and a 
  # fixed coupon rate, c, where n is the total number of months
  # and we want the breakdown for the kth period
    
  principal <- FindPayment(M, c, n) - c * BalanceLeft(M, c, n, k-1) 
  interest  <- c * BalanceLeft(M, c, n, k-1)
  output    <- list(Principal = principal, Interest = interest)
  return(output)
}

MortgagePV <- function(M, c, r, n, months = T) {
  # assuming a deterministic situation with NO defaults or prepayments
  # calculate the fair mortgage value, fv, of a mortgage, M, in an 
  # environment with interest rate, r, and coupon rate, c
  # after n periods of payment
  
  c  <- (c/12)
  fv <- (c * (1 + c)^n * M)/((1 + c)^n - 1) * ((1 + r)^n - 1)/(r * (1 + r)^n)
  return(fv) 
}

CashFlows <- function(M, c, r, n) {
  # returns the cash flows: beginning monthly balance, monthly payment,
  # interest & init balance paid per month for a mortgage, M, 
  # over n months, with fixed coupon rate, c, and fixed interest
  # rate, r
  # assumes no prepayment
  
  payment <- FindPayment(M, c, n)
  df      <- data.frame(BeginningBalLeft = rep(NA,n), Payment = NA,
                   InterestPaid = NA, PrincipalPaid = NA)
  df[1,]  <- c(M, payment, PaymentBreakdown(M, c, n, 1)[[2]], 
              PaymentBreakdown(M, c, n, 1)[[1]])
  i <- 2
  while(i < n) {
    df$BeginningBalLeft[i] <- BalanceLeft(M, c, n, i)
    df$PrincipalPaid[i] <- PaymentBreakdown(M, c, n, i)[[1]]
    df$InterestPaid[i] <- PaymentBreakdown(M, c, n, i)[[2]]
    i <- i + 1
  }
  df$Payment <- payment
  return(df)
}

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

CPR.RichardRoll <- function() {
  # built from Richard & Roll (1989) model
  # paper located here: http://www.anderson.ucla.edu/documents/areas/fac/finance/1989-1.pdf 
  
  # CPRk = refinancing incentive * seasoning multiplier * monthly multiplier * burnout multiplier
}


# Mortgage Backed Securities --------------------

MBSPassThru <- function(bal, c, r, seasoning, term, PSA) {
  # construct a data frame denoting the flows of a Pass Through MBS
  # where we define the following variables
  # seasoning = how old the mortgage pool currently is
  # bal = init balancy
  # c = annual interest paid in by mortgage holders to the institution
  # r = annual interest paid out by mortgage holders to investors
  # c > r b/c the institution assuming the risk has to be compensated (and for fees)
  # payment = init monthly payment
  # term = length of underlying mortgage(s) in months
  # PSA = multiplier for PSA 
  
  CPRvec <- vector("numeric")
  for(i in 1:term) {
    CPRvec[i] <- CPR(PSA, t = seasoning + i)
  }
  initpayment <- FindPayment(bal, c, term)
  initholdpay <- bal * (c/12)
  initinvpay  <- bal * (r/12)
  initprepay  <- (bal - (initpayment - initholdpay)) * SMM(CPRvec[1])
  df <- data.frame(CPR = CPRvec, SMM = SMM(CPRvec), BeginBal = bal, Payment = initpayment, 
                   HolderGets = initholdpay, InvestorGets = initinvpay, 
                   PrincipalPaid = initpayment - initholdpay, Prepayment = initprepay, 
                   TotalPrincipalPaid = initprepay + (initpayment - initholdpay), 
                   EndBal = bal - (initprepay + (initpayment- initholdpay)))
  
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

PrincipalPV <- function(MBS, cashrate) {
  # determine the present value of all principal payments related to a 
  # portfolio of mortgages in an input MBS, built as a data frame
  # e.g. from the above function MBSPassThru()
  # where cashrate = interest rate related to the cash flows

  principal.pv <- sum(MBS$TotalPrincipalPaid * (1 + cashrate/12)^(-(1:nrow(MBS))))
  return(principal.pv)
}

InterestPV <- function(MBS, cashrate) {
  # determine the present value of all interest payments related to a 
  # portfolio of mortgages in an input MBS, built as a data frame
  # e.g. from the above function MBSPassThru()
  # where cashrate = interest rate related to the cash flows
  
  interest.pv <- sum(MBS$InvestorGets * (1 + cashrate/12)^(-(1:nrow(MBS))))
  return(interest.pv)
}

AverageLife <- function(MBS) {
  # determines the average life of a data frame MBS object
  # as created by above function MBSPassThru()
  
  life <- crossprod((1:nrow(MBS)), MBS$TotalPrincipalPaid)/(12 * sum(MBS$TotalPrincipalPaid))
  return(life[1,1])
}

# you need to test the below
Duration <- function(M, c, r, n, principal) {
  # computes the duration of a stream of principal or interest payments 
  # for a pool of Mortgages with fair value at t=0, M, and a fixed coupon
  # rate, c, and interest rate, r, over a total of n periods
  # ASSUMES no prepayments
  
  principal.stream   <- 0
  principal.duration <- 0
  interest.stream    <- 0
  interest.duration  <- 0
    
  if(principal == T) {
    for(i in 1:n) {
      principal.stream <- principal.stream + ((i * PaymentBreakdown(M,c,n,i)[[1]])/((1+r)^i))
    }
    principal.duration <- principal.stream * ((12 * PrincipalPV(M,c,r,n))^(-1)) 
    return(principal.duration)
  } else {
    for(i in 1:n) {
      interest.stream <- interest.stream + ((i * PaymentBreakdown(M,c,n,i)[[2]])/((1+r)^i))
    }
    interest.duration <- interest.stream * ((12 * InterestPV(M,c,r,n))^(-1))
    return(interest.duration)
  }
} 

SequentialPayCMO <- function(M, c, n, ntranches, breakdown) {
  # this function is heavily reliant on the work of MBSPassThru()
  # as we're building a CMO on-top of the pass through MBS that gets constructed there
  
  # initialize tranch vectors
  for(i in 1:ntranches) {
    assign(paste("tranche.", letters[i], sep = ""),0)
  }
}

