# Amortization --------------------------

Amortization <- function(M, payment, c) {
  # calculate a timeline of balance left on a mortgage of M dollars taken over n periods
  # after a fixed payment with a coupon rate, c
  schedule <- vector("numeric")
  i <- 1
  while(M > 0) {
    schedule[i] <- M
    M <- ((1 + c)*M) - payment
    i <- i + 1
  }
  return(schedule)
}

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

PaymentBreakdown(100, .01, 10, 2)[[1]]

MortgagePV <- function(M, c, r, n) {
  # assuming a deterministic situation with NO defaults or prepayments
  # calculate the fair mortgage value, fv, of a mortgage, M, in an 
  # environment with interest rate, r, and coupon rate, c
  # after n periods of payment
  
  fv <- (c * (1 + c)^n * M)/((1 + c)^n - 1) * ((1 + r)^n - 1)/(r * (1 + r)^n)
  return(fv) 
}

CashFlows <- function(M, c, r, n) {
  
  payment <- FindPayment(M, c/12, n)
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
CashFlows(M = 100000, c = .08125, r = .08125, n = 360)

# Prepayment Risk --------------------------

