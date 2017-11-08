# Short Rate Lattice ----------------

ShortRateLattice <- function(n, r, u, b) {
  # returns a short rate lattice structure for n periods
  # with either a vector or fixed interest rate, r
  # and u being the likelihood of an up-move
  # and b the volatility parameter
  
  # in all examples q & 1-q defined as 0.5
  q <- 0.5
  d <- 0.5

  binom.lattice     <- matrix(ncol = n + 1, nrow = n + 1)
  binom.lattice[1,] <- r
  
  for(j in 1:n+1) {
    for(i in 2:j) {
      binom.lattice[i,j] <- binom.lattice[1,j] * exp(b * (i-1))
    }
  }
  return(binom.lattice)
}

# test from the spreadsheet
ShortRateLattice(13, r = 0.05, 1.25, .01)

# visualize the rate curve with the diagonal


# Cash Account ----------------------
