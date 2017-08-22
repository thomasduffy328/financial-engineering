one_period <- function(S, u, R, c = 0, strike, call) {
  
  up_move   <- 0
  down_move <- 0
  output <- list()
  d <- 1/u
  q <- (R - d - c)/(u - d)
  price <- 0
  
  if((d + c > R) | (R > u + c)) {
    # change this to throw an error
    output[1] <- "There exists an arbitrage"
  } else {
    up_move <- (u * S) + (c * S)
    down_move <- (d * S) + (c * S)
    if(call == T) {
      up_payoff <- max(up_move - strike, 0)
      down_payoff <- max(down_move - strike, 0)
    } else {
      up_payoff <- max(0, strike - up_move)
      down_payoff <- max(0, strike - down_move)
    }
    price <- (1/R) * (q * (up_payoff) + (1 - q) * (down_payoff))   
    output[1] <- up_move
    output[2] <- up_payoff
    output[3] <- down_move
    output[4] <- down_payoff
    output[5] <- price
    # should we exercise early?
    if(call == T) {
      output[6] <- price <  S - strike
    } else {
      output[6] <- price < strike - S
    }
    names(output) <- c("Up Move", "Up Pay Off", 
                       "Down Move", "Down Pay Off", "Price",
                       "Exercise Early?")
  }
  return(output)
}
