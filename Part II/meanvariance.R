# dependencies
library("ggplot2")
library("PerformanceAnalytics")
library("quantmod")
library("tseries")
library("optimx")

###### VARIANCE-COVARIANCE ######

# initiate the portfolio vector w/ exposures (weights)
PortfolioVec <- function(assets, weights) {
  stopifnot(sum(weights) == 1)
  
  vec        <- weights
  names(vec) <- assets
  return(vec)
}

RandPortfolio <- function(assets, range = 1) {
  vec <- vector(length = length(assets))
  vec <- runif(n = length(assets)-1, min = -range, max = range)
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
  return(as.numeric(100 * sqrt(t(portfoliovec[1,] %*% varcovmatrix %*% portfoliovec[1,]))))
}

RateofReturn <- function(portfoliovec, returns) {
  return((sum(portfoliovec * returns)))
}

# TO DO: implement optimization of return to solve for highest performing portfolio weights or lowest volatility

##### EFFICIENT FRONTIER #####

# TO DO: construct efficient frontier and include in plot

##### SHARPE RATIO ######

SharpePort <- function(portfolio, varcovmatrix, mu, rf, vol = 5) {
  # computes the mean excess return and the Sharpe ratio 
  # for a given portfolio and variance-covariance matrix 
  # uses Sharpe ratio as slope of Capital Market Line
  # and can compute return given a set volatility (default = 3)
  # assumes vol, rf, and mu are given in integer representation of percentages
  
  excess.return       <- mu - rf
  positions           <- excess.return %*% solve(varcovmatrix)
  sharpe.optimal.port <- PortfolioVec(assets = names(portfolio), weights = positions/sum(positions))
  mean.excess.return  <- (sharpe.optimal.port %*% excess.return) * 100 # as a percent
  sharpe.port.vol     <- PortfolioVolatility(sharpe.optimal.port, varcovmatrix)
  sharpe.ratio        <- mean.excess.return/sharpe.port.vol
  response.return     <- sharpe.ratio * vol + (rf * 100)
  statement <- paste("Return on ", vol, " % Volatility")
  
  output              <- list("Sharpe Optimal Portfolio" = sharpe.optimal.port * 100, 
                              "Mean Excess Return" = mean.excess.return, 
                              "Return" = mean.excess.return + rf,
                              "Sharpe Portfolio Volatility" = sharpe.port.vol,
                              "Sharpe Ratio" = sharpe.ratio,
                              "Return on Given Volatilty" = response.return) # fix this to print vol
  return(output)
}

##### EXAMPLES ######

# visualize random distribution of returns against their levels of volatility 
# assumption: historical returns follow Normal distribution
assets  <- c("US bonds", "int'l bonds", "US Large growth","US Large value","US Small growth", 
             "US Small value", "Int'l Dev. equity","Int'l Emerg. Equity")
returns <- c(3.15, 1.75, -6.39, -2.86,-6.75,-.54, -6.75, -5.26)
ourVar  <- c(.001,.0073, .0599, .0296, .1025, .0321,.0284,.08)

VarMat       <- VarCovMatrix(assetclasses = assets, meanreturns = returns, variances = ourVar)
ourPortfolio <- RandPortfolio(assets)
ourVol       <- PortfolioVolatility(ourPortfolio, VarMat)
PortReturns  <- RateofReturn(ourPortfolio, returns)

portfolios <- matrix(nrow = 8, ncol = 100)
for(i in 1:ncol(portfolios)) {
  portfolios[,i] <- RandPortfolio(assets)
}

returns <- apply(X = portfolios, MARGIN = 2, FUN = RateofReturn, returns = returns)
volatilies <- apply(X = portfolios, MARGIN = 2, FUN = PortfolioVolatility, varcovmatrix = VarMat)

df <- data.frame(volatilies, returns)

ggplot(df, aes(x = volatilies, y = returns)) + geom_point()
