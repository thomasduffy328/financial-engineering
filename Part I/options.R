# Binomial Option Pricing ------------------------

OnePeriod <- function(S, u, R, c = 0, strike, call, show.lattice = F) {
  # calculates the up & down moves on a binomial lattice for a call or put
  # option with strike that yields dividend, c, with R interest rate applied
  # S represents the value of the underlying asset at t=0 and u represents
  # the parameter defining the moves (and it's inverse, d)
  # show.lattice is provided for the user to either view the lattices or
  # the price & related statistics as outputs 
  
  up.move   <- 0
  down.move <- 0
  d <- 1/u
  q <- (R - d - c)/(u - d) # the risk neutral probability
  price <- 0
  
  if((d + c > R) | (R > u + c)) {
   # must first check no-arbitrage principle exists
   stop("There exists an arbitrage: we can only use no-arbitrage principal when d + c < R < u + c")
  } else {
    up.move <- (u * S) + (c * S)
    down.move <- (d * S) + (c * S)
    if(call == T) {
      up.payoff <- max(up.move - strike, 0)
      down.payoff <- max(down.move - strike, 0)
    } else { # this is our put option
      up.payoff <- max(0, strike - up.move)
      down.payoff <- max(0, strike - down.move)
    }
    price <- (1/R) * (q * (up.payoff) + (1 - q) * (down.payoff))
    
    # now construct the output list w/o the lattices
    output <- list()
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
    
    # and the option for lattices  
    lattice.moves <- matrix(nrow = 2, ncol = 2)
    lattice.payoffs <- matrix(nrow = 2, ncol = 2)
    lattice.moves[1,1] <- S
    lattice.moves[1,2] <- up.move
    lattice.moves[2,2] <- down.move
    lattice.payoffs[1,1] <- 0
    lattice.payoffs[1,2] <- up.payoff
    lattice.payoffs[2,2] <- down.payoff
    lattice.output <- list(lattice.moves, lattice.payoffs)
    names(lattice.output) <- c("Moves Lattice", "Payoffs Lattice")
  }
  
  if(show.lattice == F) {
    return(output)
  } else {
    return(lattice.output)
  }
}

MultiPeriod <- function(S, n, u, R, c = 0, strike, call) {
  # draw NA filled matrix to visualize the lattice
  
  # now calculate with a loop and fill in as you go
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
