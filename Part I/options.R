# Binomial Option Pricing ------------------------

# need to include function to solve for u

OnePeriod <- function(S, u, R, strike, call, c = 0) {
  # calculates the up & down moves on a binomial lattice for a call or put
  # option with strike that yields dividend, c, with R interest rate applied
  # S represents the value of the underlying asset at t=0 and u represents
  # the parameter defining the moves (and it's inverse, d)
  
  up.move   <- 0
  down.move <- 0
  d         <- 1/u
  q         <- (R - d - c)/(u - d) # the risk neutral probability
  price     <- 0
  
  if((d + c > R) | (R > u + c)) {
   # must first check no-arbitrage principle exists
   stop("There exists an arbitrage: we can only use no-arbitrage principal when d + c < R < u + c")
  } else {
    up.move   <- (u * S) + (c * S)
    down.move <- (d * S) + (c * S)
    if(call == T) {
      up.payoff   <- max(up.move - strike, 0)
      down.payoff <- max(down.move - strike, 0)
    } else { # this is our put option
      up.payoff   <- max(0, strike - up.move)
      down.payoff <- max(0, strike - down.move)
    }
    price <- (1/R) * (q * (up.payoff) + (1 - q) * (down.payoff))
    # prepare output list
    output <- list()
    output[1] <- S
    output[2] <- up.move
    output[3] <- up.payoff
    output[4] <- down.move
    output[5] <- down.payoff
    output[6] <- price
    # should we exercise early?
    if(call == T) {
      output[7] <- price <  S - strike
      output[8] <- max(S - strike, price)
    } else {  # put option
      output[7] <- price < strike - S
      output[8] <- max(strike - S, price)
    }
    names(output) <- c("S0", "Up Move", "Up Pay Off", 
                       "Down Move", "Down Pay Off", "Price",
                       "Exercise Early?", "Value")
  }
  return(output)
}

# FIX rounding
# you can also add option to compare value & price at each node
BinomialLattice <- function(S, n, u, R, strike, call, type = "moves", c = 0) {
  # return the binomial lattice for an option on S0 for n periods with a fixed u
  # and interest rate, R, with a set strike option
  # where call if T is a call option else a put option
  # type determines the output matrix (the default is the moves of the asset's value)
  # and c represents the dividend
  # can also output whether it's beneficial to exercise early at each node and what the 
  # option value is at each node
  
  mat        <- matrix(nrow = n + 1, ncol = n + 1)  # first column must denote asset value at t=0 (S0)
  value.mat  <- matrix(nrow = n, ncol = n)
  price.mat  <- matrix(nrow = n, ncol = n)
  early.mat  <- matrix(nrow = n, ncol = n)
  mat[1,1]   <- S

  # the up-move loop (first row)
  for(i in 2:(n+1)) {
    mat[1,i] <- OnePeriod(S = mat[1,(i-1)], u, R, strike, call, c)[[2]]
  }
  # the down-move loop (fills remainder)
  for(j in 2:(n+1)) {
    for(i in 2:j) {
      mat[i,j] <- OnePeriod(S = mat[(i-1),(j-1)], u, R, strike, call, c)[[4]]
    }
  }
  
  if(type == "exercise early") {
    for(j in 1:n) {
      for(i in 1:j) {
        early.mat[i,j] <- OnePeriod(mat[i,j], u, R, strike, call, c)[[7]]
      }
    }
    return(early.mat)
  } else if(type == "value") {
    for(j in 1:n) {
      for(i in 1:j) {
        value.mat[i,j] <- OnePeriod(mat[i,j], u, R, strike, call, c)[[8]]
      }
    }
    return(value.mat)
  } else if(type == "price") {
    for(j in 1:n) {
      for(i in 1:j) {
        price.mat[i,j] <- OnePeriod(mat[i,j], u, R, strike, call, c)[[6]]
      }
    }
    return(price.mat)
  } else {
    return(mat)
  }
}

# Black Scholes Pricing -------------------

BlackScholes <- function(S, r, u, n, sigma, maturity, strike, c = 0) {
  
  d <- 1/u
  d1 <- (log(S/strike) + ((r - c + sigma^2)/2) * maturity/sigma * sqrt(maturity))
  d2 <- d1 - sigma * sqrt(maturity)
  delta.t <- maturity/n
  q <- (e^((r - c) * (r/n)) - d)/(u - d)
  
  # need to add N(d1) and N(d2) in below
  call.price <- S * 1/(e^(c * maturity)) - strike * 1/(e^(r * maturity))
  # using put-call parity, we can calculate the put option price
  put.price <- call.price + strike * 1/(e^(r * maturity)) - S * 1/(e^(c * maturity))
  
  
}
