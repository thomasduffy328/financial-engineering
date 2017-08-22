OnePeriod <- function(S, u, R, c = 0, strike, call) {
  
  up.move   <- 0
  down.move <- 0
  output <- list()
  d <- 1/u
  q <- (R - d - c)/(u - d)
  price <- 0
  
  if((d + c > R) | (R > u + c)) {
   stop("There exists an arbitrage: we can only use no-arbitrage principal when d + c < R < u + c")
  } else {
    up.move <- (u * S) + (c * S)
    down.move <- (d * S) + (c * S)
    if(call == T) {
      up.payoff <- max(up.move - strike, 0)
      down.payoff <- max(down.move - strike, 0)
    } else {
      up.payoff <- max(0, strike - up.move)
      down.payoff <- max(0, strike - down.move)
    }
    price <- (1/R) * (q * (up.payoff) + (1 - q) * (down.payoff))   
    output[1] <- up.move
    output[2] <- up.payoff
    output[3] <- down.move
    output[4] <- down.payoff
    output[5] <- price
    # should we exercise early?
    if(call == T) {
      output[6] <- price <  S - strike
      output[7] <- max(S - strike, price)
    } else {
      output[6] <- price < strike - S
      output[7] <- max(strike - S, price)
    }
    names(output) <- c("Up Move", "Up Pay Off", 
                       "Down Move", "Down Pay Off", "Price",
                       "Exercise Early?", "Value")
  }
  return(output)
}

OnePeriod(100, 1.08, 1.1, strike = 90, call = T)

MultiPeriod <- function(orig, n, u, R, c = 0, strike, call) {
  # draw NA filled matrix to visualize the lattice
  binom.lattice <- matrix(nrow = (n + 2), ncol = (n + 2))
  
  # now calculate with a loop and fill in as you go
}

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
