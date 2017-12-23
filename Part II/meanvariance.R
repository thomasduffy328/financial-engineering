library("ggplot2")

# initiate the portfolio vector w/ exposures (weights)
PortfolioVec <- function(assets, weights) {
  stopifnot(sum(weights) == 1)
  
  vec <- weights
  names(vec) <- assets
  return(vec)
}

# also can generate random portfolio weights (useful in optimization)
RandPortfolio <- function(assets) {
  vec <- vector(length = length(assets))
  vec <- runif(n = length(assets)-1, min = -1, max = 1)
  vec[length(assets)] <- 1 - sum(vec)
  return(vec)
}

VarCovMatrix <- function(assetclasses, meanreturns, variances, n = 1000) {
  # here we are assuming a Normal distribution of returns
  
  # ensure that # asset classes is same as length of the returns vector
  stopifnot(length(assetclasses) == length(meanreturns))
  
  # construct the symmetric variance-covariance matrix
  varcovMat           <- matrix(nrow = length(assetclasses), ncol = length(assetclasses))
  colnames(varcovMat) <- assetclasses
  rownames(varcovMat) <- assetclasses
  
  # now populate with variance-covariances for the mean returns
  # their's is hard-coded
  distributions <- matrix(ncol = length(assetclasses), nrow = n)
  colnames(distributions) <- assetclasses
  for(i in 1:length(assetclasses)) {
    distributions[,i] <- rnorm(n = n, mean = meanreturns[i], sd = sqrt(variances[i]))
  }
  
  for(i in 1:length(assetclasses)) {
    for(j in 1:length(assetclasses)) {
      if(i == j) {
        varcovMat[i,i] <- variances[i]
      } else {
        varcovMat[i,j] <- var(x = distributions[,i], y = distributions[,j])
      }
    }
  }
  return(varcovMat)
}

# what's the volatility?
AssetVolatility <- function(matrix) {
  return(100 * sqrt(diag(matrix)))
}

PortfolioVolatility <- function(portfoliovec, varcovmatrix) {
  return(as.numeric(100 * sqrt(t(portfoliovec) %*% varcovmatrix %*% portfoliovec)))
}

RateofReturn <- function(portfoliovec, returns) {
  return((sum(portfoliovec * returns)))
}

# ------ Examples

# spreadsheet example (again, their's is hard-coded so won't tie out)
a <- VarCovMatrix(assetclasses = c("US bonds", "int'l bonds", "US Large growth","US Large value",
                                   "US Small growth", "US Small value", "Int'l Dev. equity",
                                   "Int'l Emerg. Equity"), meanreturns = c(3.15, 1.75, -6.39, -2.86,
                                                                           -6.75,-.54, -6.75, -5.26),
                  variances = c(.001,.0073, .0599, .0296, .1025, .0321,.0284,.08))

b <- RandPortfolio(c("US bonds", "int'l bonds", "US Large growth","US Large value",
                     "US Small growth", "US Small value", "Int'l Dev. equity",
                     "Int'l Emerg. Equity"))

# these are their hard-coded rates of return
RateofReturn(b, returns = c(3.15,1.75,-6.39,-2.86,-6.75,-0.54,-6.75,-5.26))



